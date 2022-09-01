open Core
open Hardcaml
open Signal
module Axi512 = Hardcaml_axi.Axi512

module Make (Config : Config.S) = struct
  module Top = Top.Make (Config)

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
  let axi_bits = 512

  let create ~build_mode scope { I.ap_clk; ap_rst_n; host_to_fpga; fpga_to_host_dest } =
    (* We need to decode the AXI stream input into scalars and points. *)
    let clock = ap_clk in
    let clear = ~:ap_rst_n in
    let spec = Reg_spec.create ~clock ~clear () in
    let open Always in
    let input_buffer_width = Int.round_up input_bits ~to_multiple_of:axi_bits in
    let input_l = Variable.reg spec ~width:input_buffer_width in
    let output_buffer_width =
      Int.round_up Top.result_point_bits ~to_multiple_of:axi_bits
    in
    let output_l = Variable.reg spec ~width:output_buffer_width in
    let sm = State_machine.create (module State) spec in
    let start = Variable.reg spec ~width:1 in
    let scalar_valid = Variable.reg spec ~width:1 in
    let last_scalar = Variable.reg spec ~width:1 in
    let result_point_ready = Variable.wire ~default:gnd in
    let input_point_and_scalar =
      sel_bottom input_l.value (Config.scalar_bits + Top.input_point_bits)
    in
    let top =
      Top.hierarchical
        ~build_mode
        scope
        { clock
        ; clear
        ; scalar = input_point_and_scalar.:+[0, Some Config.scalar_bits]
        ; input_point =
            input_point_and_scalar.:+[Config.scalar_bits, Some Top.input_point_bits]
        ; start = start.value
        ; scalar_valid = scalar_valid.value
        ; last_scalar = last_scalar.value
        ; result_point_ready = result_point_ready.value
        }
    in
    let valid_input_bits =
      Variable.reg spec ~width:(num_bits_to_represent input_buffer_width)
    in
    let host_to_fpga_dest = Axi512.Stream.Dest.Of_always.wire zero in
    let maybe_shift_input =
      [ host_to_fpga_dest.tready
        <-- (valid_input_bits.value +:. axi_bits <=:. input_buffer_width)
      ; when_
          (host_to_fpga.tvalid &: host_to_fpga_dest.tready.value)
          [ input_l <-- sel_top (host_to_fpga.tdata @: input_l.value) input_buffer_width
          ; valid_input_bits <-- valid_input_bits.value +:. axi_bits
          ; when_
              (valid_input_bits.value +:. axi_bits >=:. input_bits)
              [ scalar_valid <-- vdd
              ; valid_input_bits <--. 0
              ; when_ host_to_fpga.tlast [ last_scalar <-- vdd ]
              ]
          ]
      ]
      |> proc
    in
    let fpga_to_host = Axi512.Stream.Source.Of_always.reg spec in
    let valid_output_bits =
      Variable.reg spec ~width:(num_bits_to_represent output_buffer_width)
    in
    let maybe_shift_output =
      [ result_point_ready
        <-- (fpga_to_host.tvalid.value
            &: fpga_to_host_dest.tready
            |: ~:(fpga_to_host.tvalid.value))
      ; when_
          (result_point_ready.value &: top.result_point_valid)
          [ output_l <-- uresize top.result_point output_buffer_width
          ; valid_output_bits <--. Top.result_point_bits
          ; fpga_to_host.tvalid <-- vdd
          ]
      ; when_
          (fpga_to_host_dest.tready &: fpga_to_host.tvalid.value)
          [ valid_output_bits <-- valid_output_bits.value -:. axi_bits
          ; output_l <-- srl output_l.value axi_bits
          ; fpga_to_host.tvalid <-- (valid_output_bits.value >=:. axi_bits)
          ]
      ]
      |> proc
    in
    compile
      [ start <-- gnd
      ; fpga_to_host.tstrb <--. -1
      ; fpga_to_host.tkeep <--. -1
      ; fpga_to_host.tlast <--. 0
      ; when_
          top.scalar_and_input_point_ready
          [ scalar_valid <-- gnd; last_scalar <-- gnd ]
      ; when_ fpga_to_host_dest.tready [ fpga_to_host.tvalid <-- gnd ]
      ; sm.switch
          [ ( Idle
            , [ valid_input_bits <--. 0
              ; maybe_shift_input
              ; when_
                  (host_to_fpga.tvalid &: top.scalar_and_input_point_ready)
                  [ start <-- vdd ]
              ; sm.set_next Working
              ] )
          ; ( Working
            , [ maybe_shift_input
              ; maybe_shift_output
              ; when_
                  (fpga_to_host.tvalid.value
                  &: fpga_to_host.tlast.value
                  &: fpga_to_host_dest.tready)
                  [ sm.set_next Idle ]
              ] )
          ]
      ];
    { O.host_to_fpga_dest = Axi512.Stream.Dest.Of_always.value host_to_fpga_dest
    ; fpga_to_host =
        { (Axi512.Stream.Source.Of_always.value fpga_to_host) with
          tdata = sel_bottom output_l.value axi_bits
        }
    }
  ;;
end
