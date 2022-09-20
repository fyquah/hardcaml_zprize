open Core
open Hardcaml
open Signal
module Axi512 = Hardcaml_axi.Axi512
module Axi256 = Hardcaml_axi.Axi256

module Make (Config : Config.S) = struct
  module Compact_stream = Compact_stream.Make (Config)
  module Top = Top.Make (Config)
  module Msm_result_to_host = Msm_result_to_host.Make (Config)

  module I = struct
    type 'a t =
      { ap_clk : 'a
      ; ap_rst_n : 'a
      ; host_to_fpga : 'a Axi512.Stream.Source.t [@rtlprefix "host_to_fpga_"]
      ; fpga_to_host_dest : 'a Axi512.Stream.Dest.t [@rtlprefix "fpga_to_host_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { fpga_to_host : 'a Axi512.Stream.Source.t [@rtlprefix "fpga_to_host_"]
      ; host_to_fpga_dest : 'a Axi512.Stream.Dest.t [@rtlprefix "host_to_fpga_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module State = struct
    type t =
      | Idle
      | Working
    [@@deriving sexp_of, compare, enumerate]
  end

  let axi_bits = Axi256.Config.data_bits

  let input_bits =
    Int.round_up Config.scalar_bits ~to_multiple_of:axi_bits
    + Int.round_up Top.input_point_bits ~to_multiple_of:axi_bits
  ;;

  let create ~build_mode scope { I.ap_clk; ap_rst_n; host_to_fpga; fpga_to_host_dest } =
    let ( -- ) = Scope.naming scope in
    let clock = ap_clk in
    let clear = ~:ap_rst_n in
    (* We downconvert to 256 bits for SLR crossings. *)
    let compact_dn_dest = Axi256.Stream.Dest.Of_always.wire zero in
    let compact_stream =
      Compact_stream.hierarchical
        scope
        { clock
        ; clear
        ; up = host_to_fpga
        ; dn_dest = Axi256.Stream.Dest.Of_always.value compact_dn_dest
        }
    in
    let spec = Reg_spec.create ~clock ~clear () in
    let open Always in
    (* Two input buffers so that we can keep the controller fed with points. *)
    let buffer_w_select = Variable.reg spec ~width:1 in
    let buffer_r_select = Variable.reg spec ~width:1 in
    let input_l0 = Variable.reg spec ~width:input_bits in
    let input_l1 = Variable.reg spec ~width:input_bits in
    let tlast0 = Variable.reg spec ~width:1 in
    let tlast1 = Variable.reg spec ~width:1 in
    let scalar_valid0 = Variable.reg spec ~width:1 in
    let scalar_valid1 = Variable.reg spec ~width:1 in
    let sm = State_machine.create (module State) spec in
    let result_point_ready = wire 1 in
    let input_point_and_scalar =
      sel_bottom
        (mux2 buffer_r_select.value input_l1.value input_l0.value)
        (Config.scalar_bits + Top.input_point_bits)
    in
    let top =
      Top.hierarchical
        ~build_mode
        scope
        { clock
        ; clear
        ; scalar =
            input_point_and_scalar.:+[Top.input_point_bits, Some Config.scalar_bits]
        ; input_point =
            Top.Mixed_add.Xyt.Of_signal.unpack
              input_point_and_scalar.:+[0, Some Top.input_point_bits]
        ; scalar_valid =
            mux2 buffer_r_select.value scalar_valid1.value scalar_valid0.value
        ; last_scalar = mux2 buffer_r_select.value tlast1.value tlast0.value
        ; result_point_ready
        }
    in
    let msm_result_to_host =
      Msm_result_to_host.hierarchical
        scope
        ~drop_t:gnd
        { clock
        ; clear
        ; result_point =
            { x = top.result_point.x
            ; y = top.result_point.y
            ; z = top.result_point.z
            ; t = top.result_point.t
            }
        ; result_point_valid = top.result_point_valid
        ; last_result_point = top.last_result_point
        ; fpga_to_host_dest
        }
    in
    result_point_ready <== msm_result_to_host.result_point_ready;
    let valid_input_bits0 = Variable.reg spec ~width:(num_bits_to_represent input_bits) in
    let valid_input_bits1 = Variable.reg spec ~width:(num_bits_to_represent input_bits) in
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
          (scalar_valid1.value |: scalar_valid0.value &: top.scalar_and_input_point_ready)
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
    ignore (sm.current -- "state" : Signal.t);
    compile
      [ when_
          top.scalar_and_input_point_ready
          [ if_
              buffer_r_select.value
              [ scalar_valid1 <-- gnd; tlast1 <-- gnd ]
              [ scalar_valid0 <-- gnd; tlast0 <-- gnd ]
          ]
      ; sm.switch
          [ ( Idle
            , [ valid_input_bits0 <--. 0
              ; valid_input_bits1 <--. 0
              ; buffer_w_select <--. 0
              ; buffer_r_select <--. 0
              ; scalar_valid0 <-- gnd
              ; scalar_valid1 <-- gnd
              ; when_ compact_stream.dn.tvalid [ sm.set_next Working ]
              ] )
          ; ( Working
            , [ maybe_shift_input
              ; when_
                  (msm_result_to_host.fpga_to_host.tvalid
                  &: msm_result_to_host.fpga_to_host.tlast
                  &: fpga_to_host_dest.tready)
                  [ sm.set_next Idle ]
              ] )
          ]
      ];
    { O.host_to_fpga_dest = compact_stream.up_dest
    ; fpga_to_host = msm_result_to_host.fpga_to_host
    }
  ;;

  let hierarchical ~build_mode scope =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~name:"kernel_for_vitis" ~scope (create ~build_mode)
  ;;
end
