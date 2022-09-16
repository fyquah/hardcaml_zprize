open Base
open Hardcaml
open Signal

module Make (Config : Hardcaml_ntt.Ntt_4step.Config) = struct
  include Config
  module Ntt_4step = Hardcaml_ntt.Ntt_4step.Make (Config)
  module Axi_stream = Ntt_4step.Axi_stream
  module Gf = Ntt_4step.Gf
  module Transposer = Hardcaml_ntt.Transposer

  let cores = 1 lsl logcores

  module Kernel = struct
    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; start : 'a
        ; first_4step_pass : 'a
        ; data_in : 'a Axi_stream.Source.t
        ; data_out_dest : 'a Axi_stream.Dest.t
        }
      [@@deriving sexp_of, hardcaml ~rtlmangle:true]
    end

    module O = struct
      type 'a t =
        { data_out : 'a Axi_stream.Source.t
        ; data_in_dest : 'a Axi_stream.Dest.t
        ; done_ : 'a
        }
      [@@deriving sexp_of, hardcaml ~rtlmangle:true]
    end

    module Var = Always.Variable

    module Load_sm = struct
      module State = struct
        type t =
          | Start
          | Stream
        [@@deriving sexp_of, compare, enumerate]
      end

      type 'a t =
        { done_ : 'a
        ; tready : 'a
        ; wr_addr : 'a
        ; wr_en : 'a
        }
      [@@deriving sexp_of, hardcaml]

      let create (i : _ I.t) ~start =
        let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
        let sm = Always.State_machine.create (module State) spec in
        let addr = Var.reg spec ~width:logn in
        let addr_next = addr.value +:. 1 in
        Always.(
          compile
            [ sm.switch
                [ Start, [ addr <--. 0; when_ start [ sm.set_next Stream ] ]
                ; ( Stream
                  , [ when_
                        i.data_in.tvalid
                        [ addr <-- addr_next
                        ; when_ (addr_next ==:. 0) [ sm.set_next Start ]
                        ]
                    ] )
                ]
            ]);
        let done_ = sm.is Start in
        let processing = ~:done_ in
        { done_
        ; tready = processing
        ; wr_en = processing &: i.data_in.tvalid
        ; wr_addr = addr.value
        }
      ;;
    end

    module Store_sm = struct
      module State = struct
        type t =
          | Start
          | Preroll
          | Stream
        [@@deriving sexp_of, compare, enumerate]
      end

      type 'a t =
        { done_ : 'a
        ; tvalid : 'a
        ; rd_addr : 'a
        ; rd_en : 'a
        }
      [@@deriving sexp_of, hardcaml]

      let create (i : _ I.t) ~start =
        let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
        let sm = Always.State_machine.create (module State) spec in
        let addr = Var.reg spec ~width:(logn + 1) in
        let addr_next = addr.value +:. 1 in
        let rd_en = Var.wire ~default:gnd in
        let tvalid = Var.reg spec ~width:1 in
        (* XXX fixme *)
        Always.(
          compile
            [ sm.switch
                [ Start, [ addr <--. 0; when_ start [ sm.set_next Preroll ] ]
                ; ( Preroll
                  , [ rd_en <-- vdd; addr <--. 1; tvalid <-- vdd; sm.set_next Stream ] )
                ; ( Stream
                  , [ when_
                        i.data_out_dest.tready
                        [ addr <-- addr_next
                        ; rd_en <-- vdd
                        ; when_
                            (addr.value ==:. 1 lsl logn)
                            [ tvalid <-- gnd; sm.set_next Start ]
                        ]
                    ] )
                ]
            ]);
        let done_ = sm.is Start in
        { done_
        ; tvalid = tvalid.value
        ; rd_addr = addr.value.:[logn - 1, 0]
        ; rd_en = rd_en.value
        }
      ;;
    end

    let num_cores = cores

    let create ~build_mode scope (i : _ I.t) =
      let start_input = wire 1 in
      let start_output = wire 1 in
      let load_sm = Load_sm.create i ~start:start_input in
      let store_sm = Store_sm.create i ~start:start_output in
      let cores =
        Ntt_4step.Core.create
          ~build_mode
          scope
          { Ntt_4step.Core.I.clock = i.clock
          ; clear = i.clear
          ; start = i.start
          ; first_4step_pass = i.first_4step_pass
          ; wr_d = i.data_in.tdata |> split_lsb ~part_width:Gf.num_bits |> Array.of_list
          ; wr_en = repeat (i.data_in.tvalid &: load_sm.tready) cores
          ; wr_addr = load_sm.wr_addr
          ; rd_en = repeat store_sm.rd_en cores
          ; rd_addr = store_sm.rd_addr
          ; input_done = load_sm.done_
          ; output_done = store_sm.done_
          }
      in
      start_input <== cores.start_input;
      start_output <== cores.start_output;
      { O.data_out =
          { tvalid = store_sm.tvalid
          ; tdata = cores.rd_q |> Array.to_list |> concat_lsb
          ; tlast = gnd
          ; tkeep = ones (num_cores * Gf.num_bits / 8)
          ; tstrb = ones (num_cores * Gf.num_bits / 8)
          }
      ; data_in_dest = { tready = load_sm.tready }
      ; done_ = cores.done_
      }
    ;;

    let hierarchy ~build_mode scope =
      let module Hier = Hierarchy.In_scope (I) (O) in
      Hier.hierarchical ~name:"kernel" ~scope (create ~build_mode)
    ;;
  end

  module Kernel_for_vitis = struct
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
      let { Kernel.O.data_out; data_in_dest = data_in_dest'; done_ } =
        Kernel.hierarchy
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
end
