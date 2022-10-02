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
  let axi_bits = 512

  (* We assume the scalar can fit in the DDR words the point uses. This is asserted in the merge stream module. *)
  let input_bits = Int.round_up xyt_aligned_bits ~to_multiple_of:axi_bits

  let create scope ({ clock; clear; host_to_fpga; scalar_and_input_point_ready } : _ I.t)
    : _ O.t
    =
    let ( -- ) = Scope.naming scope in
    let spec = Reg_spec.create ~clock () in
    let spec_with_clear = Reg_spec.create ~clock ~clear () in
    let open Always in
    let input_l = Variable.reg spec ~width:input_bits in
    let tlast = Variable.reg spec_with_clear ~width:1 in
    let scalar_valid = Variable.reg spec_with_clear ~width:1 in
    let input_point_and_scalar =
      sel_bottom input_l.value (xyt_aligned_bits + Config.scalar_bits)
    in
    let valid_input_bits =
      Variable.reg spec_with_clear ~width:(num_bits_to_represent (input_bits + axi_bits))
    in
    let host_to_fpga_dest = Axi512.Stream.Dest.Of_always.wire zero in
    ignore (valid_input_bits.value -- "valid_input_bits" : Signal.t);
    compile
      [ when_ scalar_and_input_point_ready [ scalar_valid <-- gnd; tlast <-- gnd ]
      ; host_to_fpga_dest.tready <-- (valid_input_bits.value <:. input_bits)
      ; when_
          (host_to_fpga.tvalid &: host_to_fpga_dest.tready.value)
          [ input_l <-- sel_top (host_to_fpga.tdata @: input_l.value) input_bits
          ; when_
              (valid_input_bits.value +:. axi_bits >=:. input_bits)
              [ scalar_valid <-- vdd; when_ host_to_fpga.tlast [ tlast <-- vdd ] ]
          ]
      ; valid_input_bits
        <-- valid_input_bits.value
            -: mux2
                 (scalar_valid.value &: scalar_and_input_point_ready)
                 valid_input_bits.value
                 (zero (width valid_input_bits.value))
            +: mux2
                 (host_to_fpga.tvalid &: host_to_fpga_dest.tready.value)
                 (of_int axi_bits ~width:(width valid_input_bits.value))
                 (zero (width valid_input_bits.value))
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
    { O.host_to_fpga_dest = Axi512.Stream.Dest.Of_always.value host_to_fpga_dest
    ; scalar
    ; input_point
    ; scalar_valid = scalar_valid.value
    ; last_scalar = tlast.value
    }
  ;;

  let hierarchical ?instance scope =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ?instance ~name:"host_to_msm_simple" ~scope create
  ;;
end
