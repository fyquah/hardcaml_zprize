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

  let input_bits = Config.scalar_bits + Top.input_point_bits
  let axi_bits = Axi256.Config.data_bits
  let alignment_bits = 256
  let log2_alignment_bits = Int.ceil_log2 alignment_bits
  let input_l_bits = Int.round_up ~to_multiple_of:axi_bits input_bits

  let create ~build_mode scope { I.ap_clk; ap_rst_n; host_to_fpga; fpga_to_host_dest } =
    let ( -- ) = Scope.naming scope in
    let clock = ap_clk in
    let clear = ~:ap_rst_n in
    (* We downconvert to 256 bits for SLR crossings. *)
    let compact_dn_dest = Axi256.Stream.Dest.Of_always.wire zero in
    let compact_stream =
      Compact_stream.create
        scope
        { clock
        ; clear
        ; up = host_to_fpga
        ; dn_dest = Axi256.Stream.Dest.Of_always.value compact_dn_dest
        }
    in
    let spec = Reg_spec.create ~clock ~clear () in
    let open Always in
    let input_l = Variable.reg spec ~width:input_l_bits in
    let sm = State_machine.create (module State) spec in
    let scalar_valid = Variable.reg spec ~width:1 in
    let last_scalar = Variable.reg spec ~width:1 in
    let result_point_ready = wire 1 in
    let input_offset =
      Variable.reg spec ~width:(num_bits_to_represent (axi_bits / alignment_bits))
    in
    let input_point_and_scalar =
      sel_bottom
        (log_shift srl input_l.value (sll input_offset.value log2_alignment_bits))
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
        ; scalar_valid = scalar_valid.value
        ; last_scalar = last_scalar.value
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
    let valid_input_bits =
      Variable.reg spec ~width:(num_bits_to_represent input_l_bits)
    in
    let maybe_shift_input =
      [ compact_dn_dest.tready <-- (valid_input_bits.value <:. input_bits)
      ; when_
          (compact_stream.dn.tvalid &: compact_dn_dest.tready.value)
          [ input_l <-- sel_top (compact_stream.dn.tdata @: input_l.value) input_l_bits
          ; valid_input_bits <-- valid_input_bits.value +:. axi_bits
          ; when_
              (valid_input_bits.value +:. axi_bits >=:. input_bits)
              [ scalar_valid <-- vdd
              ; when_ compact_stream.dn.tlast [ last_scalar <-- vdd ]
              ]
          ]
      ; when_
          (scalar_valid.value &: top.scalar_and_input_point_ready)
          [ input_offset
            <-- sll
                  (srl
                     (input_offset.value +:. input_bits +:. alignment_bits)
                     log2_alignment_bits)
                  log2_alignment_bits
          ; valid_input_bits <--. 0
          ]
      ]
      |> proc
    in
    ignore (input_l.value -- "input_l" : Signal.t);
    ignore (input_offset.value -- "input_offset" : Signal.t);
    ignore (valid_input_bits.value -- "valid_input_bits" : Signal.t);
    ignore (sm.current -- "state" : Signal.t);
    compile
      [ when_
          top.scalar_and_input_point_ready
          [ scalar_valid <-- gnd; last_scalar <-- gnd ]
      ; sm.switch
          [ ( Idle
            , [ valid_input_bits <--. 0
              ; input_offset <--. 0
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
