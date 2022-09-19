open Core
open Hardcaml
open Signal
module Axi512 = Hardcaml_axi.Axi512

module Make (C : Config.S) = struct
  let config = C.t

  let { Config.field_bits
      ; scalar_bits_by_core
      ; controller_log_stall_fifo_depth
      ; window_size_bits
      ; ram_read_latency
      }
    =
    config
  ;;

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

    let names =
      List.map all ~f:(function
        | Idle -> "I"
        | Working -> "W")
    ;;
  end

  let scalar_bits = Config.scalar_bits config
  let input_point_bits = Config.input_point_bits config
  let result_point_bits = Config.result_point_bits config
  let input_bits = scalar_bits + input_point_bits
  let axi_bits = Axi512.Config.data_bits
  let alignment_bits = 512
  let log2_alignment_bits = Int.ceil_log2 alignment_bits
  let input_l_bits = Int.round_up ~to_multiple_of:axi_bits input_bits
  let output_l_width = Int.round_up result_point_bits ~to_multiple_of:axi_bits

  let scalar_bit_positions ~core_index =
    let lo = ref 0 in
    for i = 0 to core_index - 1 do
      lo := !lo + scalar_bits_by_core.(i)
    done;
    let w = scalar_bits_by_core.(core_index) in
    !lo + w - 1, !lo
  ;;

  type comppute_unit =
    { scalar_and_input_point_ready : Signal.t
    ; result_point : Signal.t
    ; result_point_valid : Signal.t
    ; last_result_point : Signal.t
    }

  let create
    ~build_mode
    ~core_index
    scope
    { I.ap_clk; ap_rst_n; host_to_fpga; fpga_to_host_dest }
    =
    let ( -- ) = Scope.naming scope in
    (* We need to decode the AXI stream input into scalars and points. *)
    let clock = ap_clk in
    let clear = ~:ap_rst_n in
    let spec = Reg_spec.create ~clock ~clear () in
    let open Always in
    let input_l = Variable.reg spec ~width:input_l_bits in
    let output_l = Variable.reg spec ~width:output_l_width in
    let sm = State_machine.create (module State) spec in
    let scalar_valid = Variable.reg spec ~width:1 in
    let last_scalar = Variable.reg spec ~width:1 in
    let result_point_ready = Variable.wire ~default:gnd in
    let input_offset =
      Variable.reg spec ~width:(num_bits_to_represent (axi_bits / alignment_bits))
    in
    let input_point_and_scalar =
      sel_bottom
        (log_shift srl input_l.value (sll input_offset.value log2_alignment_bits))
        (scalar_bits + input_point_bits)
    in
    let top =
      let module Pippenger_compute_unit =
        Pippenger_compute_unit.Make (struct
          let t =
            { Pippenger_compute_unit.Pippenger_compute_unit_config.field_bits
            ; scalar_bits = scalar_bits_by_core.(core_index)
            ; controller_log_stall_fifo_depth
            ; window_size_bits
            ; ram_read_latency
            }
          ;;
        end)
      in
      let scalar =
        let hi, lo = scalar_bit_positions ~core_index in
        input_point_and_scalar.:[input_point_bits + hi, input_point_bits + lo]
      in
      let { Pippenger_compute_unit.O.result_point
          ; result_point_valid
          ; last_result_point
          ; scalar_and_input_point_ready
          }
        =
        Pippenger_compute_unit.hierarchical
          ~build_mode
          scope
          { clock
          ; clear
          ; scalar
          ; input_point =
              Pippenger_compute_unit.Mixed_add.Xyt.Of_signal.unpack
                input_point_and_scalar.:+[0, Some Pippenger_compute_unit.input_point_bits]
          ; scalar_valid = scalar_valid.value
          ; last_scalar = last_scalar.value
          ; result_point_ready = result_point_ready.value
          }
      in
      { scalar_and_input_point_ready
      ; result_point = Pippenger_compute_unit.Mixed_add.Xyzt.Of_signal.pack result_point
      ; result_point_valid
      ; last_result_point
      }
    in
    let valid_input_bits =
      Variable.reg spec ~width:(num_bits_to_represent input_l_bits)
    in
    let host_to_fpga_dest = Axi512.Stream.Dest.Of_always.wire zero in
    let maybe_shift_input =
      [ host_to_fpga_dest.tready <-- (valid_input_bits.value <:. input_bits)
      ; when_
          (host_to_fpga.tvalid &: host_to_fpga_dest.tready.value)
          [ input_l <-- sel_top (host_to_fpga.tdata @: input_l.value) input_l_bits
          ; valid_input_bits <-- valid_input_bits.value +:. axi_bits
          ; when_
              (valid_input_bits.value +:. axi_bits >=:. input_bits)
              [ scalar_valid <-- vdd; when_ host_to_fpga.tlast [ last_scalar <-- vdd ] ]
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
    let fpga_to_host = Axi512.Stream.Source.Of_always.reg spec in
    let valid_output_bits =
      Variable.reg spec ~width:(num_bits_to_represent output_l_width)
    in
    ignore (valid_output_bits.value -- "valid_output_bits" : Signal.t);
    let output_buffer_valid = Variable.reg spec ~width:1 in
    let last_l = Variable.reg spec ~width:1 in
    let maybe_shift_output =
      [ when_
          (fpga_to_host_dest.tready &: fpga_to_host.tvalid.value)
          [ valid_output_bits <-- valid_output_bits.value -:. axi_bits
          ; output_l <-- srl output_l.value axi_bits
          ; fpga_to_host.tvalid <-- (valid_output_bits.value >=:. axi_bits)
          ; fpga_to_host.tlast
            <-- (last_l.value &: (valid_output_bits.value <=:. 2 * axi_bits))
          ]
      ; when_
          (~:(output_buffer_valid.value)
          |: (valid_output_bits.value
             <=:. axi_bits
             &: fpga_to_host_dest.tready
             &: fpga_to_host.tvalid.value))
          [ output_buffer_valid <-- gnd
          ; valid_output_bits <--. 0
          ; result_point_ready <-- vdd
          ; output_l <-- uresize top.result_point output_l_width
          ; when_
              (top.result_point_valid &: result_point_ready.value)
              [ output_buffer_valid <-- vdd
              ; fpga_to_host.tvalid <-- vdd
              ; valid_output_bits <--. result_point_bits
              ]
          ]
      ; when_
          (result_point_ready.value &: top.result_point_valid)
          [ last_l <-- top.last_result_point ]
      ]
      |> proc
    in
    ignore (sm.current -- "state" : Signal.t);
    compile
      [ fpga_to_host.tstrb <--. -1
      ; fpga_to_host.tkeep <--. -1
      ; fpga_to_host.tlast <--. 0
      ; when_
          top.scalar_and_input_point_ready
          [ scalar_valid <-- gnd; last_scalar <-- gnd ]
      ; when_ fpga_to_host_dest.tready [ fpga_to_host.tvalid <-- gnd ]
      ; sm.switch
          [ ( Idle
            , [ valid_input_bits <--. 0
              ; last_l <-- gnd
              ; input_offset <--. 0
              ; when_ host_to_fpga.tvalid [ sm.set_next Working ]
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

  let hierarchical ~build_mode ~core_index scope =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~name:"kernel_for_vitis" ~scope (create ~core_index ~build_mode)
  ;;
end
