open Base
open Hardcaml
open Signal

module Make (Config : Top_config.S) = struct
  include Config
  module Four_step = Hardcaml_ntt.Four_step.Make (Config)
  module Axi_stream = Four_step.Axi_stream
  module Transposer = Hardcaml_ntt.Transposer
  module Top = Top.Make (Config)

  module I = struct
    type 'a t =
      { ap_clk : 'a
      ; ap_rst_n : 'a
      ; controller_to_compute_phase_1 : 'a Axi_stream.Source.t [@rtlmangle true]
      ; controller_to_compute_phase_2 : 'a Axi_stream.Source.t [@rtlmangle true]
      ; compute_to_controller_dest : 'a Axi_stream.Dest.t
           [@rtlprefix "compute_to_controller_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { compute_to_controller : 'a Axi_stream.Source.t [@rtlmangle true]
      ; controller_to_compute_phase_1_dest : 'a Axi_stream.Dest.t
           [@rtlprefix "controller_to_compute_phase_1_"]
      ; controller_to_compute_phase_2_dest : 'a Axi_stream.Dest.t
           [@rtlprefix "controller_to_compute_phase_2_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create
    ~build_mode
    scope
    { I.ap_clk = clock
    ; ap_rst_n = clear_n
    ; controller_to_compute_phase_1
    ; controller_to_compute_phase_2
    ; compute_to_controller_dest
    }
    =
    let clear = ~:clear_n in
    let spec = Reg_spec.create ~clock ~clear () in
    let start = wire 1 in
    let transposer_out_dest = Axi_stream.Dest.Of_signal.wires () in
    let transposer =
      Transposer.hierarchical
        ~transposer_depth_in_cycles:1
        scope
        { clock
        ; clear
        ; transposer_in =
            (let { Axi_stream.Source.tvalid; tdata; tkeep; tstrb; tlast } =
               controller_to_compute_phase_2
             in
             assert (width tdata = 512);
             { tvalid; tdata; tkeep; tstrb; tlast })
        ; transposer_out_dest =
            (let { Axi_stream.Dest.tready } = transposer_out_dest in
             { tready })
        }
    in
    let transposer_out =
      let { Transposer.Axi512.Stream.Source.tvalid; tdata; tkeep; tstrb; tlast } =
        transposer.transposer_out
      in
      { Axi_stream.Source.tvalid; tdata; tkeep; tstrb; tlast }
    in
    let first_4step_pass = reg_fb spec ~enable:start ~width:1 ~f:( ~: ) -- "4STEP" in
    let data_in_dest = Axi_stream.Dest.Of_signal.wires () in
    let { Top.O.data_out; data_in_dest = data_in_dest'; done_ } =
      Top.hierarchy
        ~build_mode
        scope
        { clock
        ; clear
        ; first_4step_pass
        ; data_in =
            (* Strictly speaking, the following register is not AXI spec
             * compliant. However, our Load_sm will readily assert tready
             * even if tvalid has not been asserted yet, so we can cheat here
             * and get away with the following simple pipelining logic
             * (rather than a full-fledged datapath register).
             *)
            Axi_stream.Source.Of_signal.mux2
              controller_to_compute_phase_1.tvalid
              controller_to_compute_phase_1
              transposer_out
        ; data_out_dest = compute_to_controller_dest
        ; start
        }
    in
    (* Note that the transposer.out and controller_to_compute_phase1 shares
     * the same dest. This is okay, because we statically know that the
     * C++ krnl_controller will never stream both things simultaneously.
     *)
    Axi_stream.Dest.Of_signal.( <== ) transposer_out_dest data_in_dest;
    Axi_stream.Dest.Of_signal.( <== ) data_in_dest data_in_dest';
    start
    <== (done_
        &: (controller_to_compute_phase_1.tvalid |: controller_to_compute_phase_2.tvalid)
        );
    { O.compute_to_controller = data_out
    ; controller_to_compute_phase_1_dest = data_in_dest
    ; controller_to_compute_phase_2_dest =
        { tready = transposer.transposer_in_dest.tready }
    }
  ;;
end
