open Core
open Hardcaml
open Hardcaml_axi
open Signal

module Make (Config : Config.S) = struct
  module Mixed_add = Twisted_edwards_lib.Mixed_add.Make (struct
    let num_bits = Config.field_bits
  end)

  module Compact_stream = Compact_stream.Make (Config)
  module Top = Top.Make (Config)
  module Xyt = Top.Mixed_add.Xyt

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; host_to_fpga : 'a Axi512.Stream.Source.t
      ; scalar_and_input_point_ready : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { host_to_fpga_dest : 'a Axi512.Stream.Dest.t
      ; scalar : 'a [@bits Config.scalar_bits]
      ; scalar_valid : 'a
      ; last_scalar : 'a
      ; input_point : 'a Top.Mixed_add.Xyt.t [@rtl_prefix "i_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let aligned_to = 64

  module Xyt_aligned =
    Interface.Update
      (Xyt)
      (struct
        let t =
          Xyt.(
            map2 port_names port_widths ~f:(fun n w ->
              n, Int.round_up w ~to_multiple_of:aligned_to))
        ;;
      end)

  let xyt_aligned_bits = Xyt_aligned.(fold port_widths ~init:0 ~f:( + ))
  let axi_bits = Axi256.Config.data_bits

  let input_bits =
    Int.round_up Config.scalar_bits ~to_multiple_of:axi_bits
    + Int.round_up (Config.field_bits * 3) ~to_multiple_of:axi_bits
  ;;

  (* Make sure this alignment does not break the compactor *)
  let () =
    assert (
      input_bits
      >= xyt_aligned_bits + Int.round_up Config.scalar_bits ~to_multiple_of:aligned_to)
  ;;

  let create scope (i : _ I.t) : _ O.t =
    let ( -- ) = Scope.naming scope in
    (* We downconvert to 256 bits for SLR crossings. *)
    let compact_dn_dest = Axi256.Stream.Dest.Of_always.wire zero in
    let compact_stream =
      Compact_stream.hierarchical
        scope
        { clock = i.clock
        ; clear = i.clear
        ; up = i.host_to_fpga
        ; dn_dest = Axi256.Stream.Dest.Of_always.value compact_dn_dest
        }
    in
    let spec = Reg_spec.create ~clock:i.clock () in
    let spec_with_clear = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let open Always in
    (* Two input buffers so that we can keep the controller fed with points. *)
    let buffer_w_select = Variable.reg spec_with_clear ~width:1 in
    let buffer_r_select = Variable.reg spec_with_clear ~width:1 in
    let input_l0 = Variable.reg spec ~width:input_bits in
    let input_l1 = Variable.reg spec ~width:input_bits in
    let tlast0 = Variable.reg spec_with_clear ~width:1 in
    let tlast1 = Variable.reg spec_with_clear ~width:1 in
    let scalar_valid0 = Variable.reg spec_with_clear ~width:1 in
    let scalar_valid1 = Variable.reg spec_with_clear ~width:1 in
    let input_point_and_scalar =
      sel_bottom
        (mux2 buffer_r_select.value input_l1.value input_l0.value)
        (xyt_aligned_bits + Config.scalar_bits)
    in
    let valid_input_bits0 =
      Variable.reg spec_with_clear ~width:(num_bits_to_represent input_bits)
    in
    let valid_input_bits1 =
      Variable.reg spec_with_clear ~width:(num_bits_to_represent input_bits)
    in
    let maybe_shift_input =
      [ compact_dn_dest.tready
        <-- (mux2 buffer_w_select.value valid_input_bits1.value valid_input_bits0.value
            <:. input_bits)
      ; when_
          (compact_stream.dn.tvalid &: compact_dn_dest.tready.value)
          [ if_
              buffer_w_select.value
              [ input_l1
                <-- sel_top (compact_stream.dn.tdata @: input_l1.value) input_bits
              ; valid_input_bits1 <-- valid_input_bits1.value +:. axi_bits
              ; when_
                  (valid_input_bits1.value +:. axi_bits >=:. input_bits)
                  [ scalar_valid1 <-- vdd
                  ; buffer_w_select <-- ~:(buffer_w_select.value)
                  ; when_ compact_stream.dn.tlast [ tlast1 <-- vdd ]
                  ]
              ]
              [ input_l0
                <-- sel_top (compact_stream.dn.tdata @: input_l0.value) input_bits
              ; valid_input_bits0 <-- valid_input_bits0.value +:. axi_bits
              ; when_
                  (valid_input_bits0.value +:. axi_bits >=:. input_bits)
                  [ scalar_valid0 <-- vdd
                  ; buffer_w_select <-- ~:(buffer_w_select.value)
                  ; when_ compact_stream.dn.tlast [ tlast0 <-- vdd ]
                  ]
              ]
          ]
      ; when_
          (scalar_valid1.value |: scalar_valid0.value &: i.scalar_and_input_point_ready)
          [ buffer_r_select <-- ~:(buffer_r_select.value)
          ; if_
              buffer_r_select.value
              [ valid_input_bits1 <--. 0 ]
              [ valid_input_bits0 <--. 0 ]
          ]
      ]
      |> proc
    in
    ignore (input_l0.value -- "input_l" : Signal.t);
    ignore (valid_input_bits0.value -- "valid_input_bits" : Signal.t);
    ignore (buffer_r_select.value -- "buffer_r_select" : Signal.t);
    ignore (buffer_w_select.value -- "buffer_w_select" : Signal.t);
    ignore (scalar_valid0.value -- "scalar_valid0" : Signal.t);
    ignore (scalar_valid1.value -- "scalar_valid1" : Signal.t);
    compile
      [ when_
          i.scalar_and_input_point_ready
          [ if_
              buffer_r_select.value
              [ scalar_valid1 <-- gnd; tlast1 <-- gnd ]
              [ scalar_valid0 <-- gnd; tlast0 <-- gnd ]
          ]
      ; maybe_shift_input
      ];
    let input_point =
      let aligned_point =
        Xyt_aligned.Of_signal.unpack (sel_bottom input_point_and_scalar xyt_aligned_bits)
      in
      { Xyt.x = sel_bottom aligned_point.x Config.field_bits
      ; y = sel_bottom aligned_point.y Config.field_bits
      ; t = sel_bottom aligned_point.t Config.field_bits
      }
    in
    let scalar =
      select
        input_point_and_scalar
        (Config.scalar_bits + xyt_aligned_bits - 1)
        xyt_aligned_bits
    in
    { O.host_to_fpga_dest = compact_stream.up_dest
    ; scalar
    ; input_point
    ; scalar_valid = mux2 buffer_r_select.value scalar_valid1.value scalar_valid0.value
    ; last_scalar = mux2 buffer_r_select.value tlast1.value tlast0.value
    }
  ;;

  let hierarchical ?instance scope =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ?instance ~name:"host_to_msm" ~scope create
  ;;
end
