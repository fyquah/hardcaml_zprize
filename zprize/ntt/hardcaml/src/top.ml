open Base
open Hardcaml
open Signal

module Make (Config : Top_config.S) = struct
  include Config
  module Four_step = Hardcaml_ntt.Four_step.Make (Config)
  module Axi_stream = Four_step.Axi_stream
  module Gf = Four_step.Gf
  module Load_sm = Load_sm.Make (Config)
  module Store_sm = Store_sm.Make (Config)

  let cores = 1 lsl logcores
  let blocks = 1 lsl logblocks

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a (** Start running a pass. *)
      ; first_4step_pass : 'a (** High is running the first pass. *)
      ; data_in : 'a Axi_stream.Source.t
      ; data_out_dest : 'a Axi_stream.Dest.t
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:true]
  end

  module O = struct
    type 'a t =
      { data_out : 'a Axi_stream.Source.t
      ; data_in_dest : 'a Axi_stream.Dest.t
      ; done_ : 'a (** Low while processing a pass. *)
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:true]
  end

  module Var = Always.Variable

  let num_cores = cores

  let create ~build_mode scope (i : _ I.t) =
    let start_input = wire 1 in
    let start_output = wire 1 in
    let load_sm =
      Load_sm.hierarchy
        scope
        { Load_sm.I.clock = i.clock
        ; clear = i.clear
        ; first_4step_pass = i.first_4step_pass
        ; tvalid = i.data_in.tvalid
        ; start = start_input
        }
    in
    let store_sm =
      Store_sm.hierarchy
        scope
        { Store_sm.I.clock = i.clock
        ; clear = i.clear
        ; first_4step_pass = i.first_4step_pass
        ; tready = i.data_out_dest.tready
        ; start = start_output
        }
    in
    let pipe = pipeline (Reg_spec.create ~clock:i.clock ()) in
    let pipe_with_keep ?enable ~n x =
      Signal.pipeline
        ?enable
        ~attributes:[ Rtl_attribute.Vivado.keep true ]
        (Reg_spec.create ~clock:i.clock ())
        ~n
        x
    in
    let cores =
      Four_step.hierarchy
        ~build_mode
        scope
        { Four_step.I.clock = i.clock
        ; clear = i.clear
        ; start = i.start
        ; first_4step_pass = i.first_4step_pass
        ; wr_d =
            (let d =
               i.data_in.tdata
               |> pipe ~n:2
               |> split_lsb ~part_width:Gf.num_bits
               |> Array.of_list
             in
             Array.init blocks ~f:(Fn.const d))
        ; wr_en = pipe_with_keep ~n:Load_sm.write_pipelining load_sm.wr_en
        ; wr_addr =
            Array.init blocks ~f:(fun _ ->
              pipe_with_keep ~n:Load_sm.write_pipelining load_sm.wr_addr)
        ; rd_en =
            pipe_with_keep
              ~enable:store_sm.rd_any
              ~n:Store_sm.read_address_pipelining
              store_sm.rd_en
            &: repeat store_sm.rd_any (width store_sm.rd_en)
        ; rd_addr =
            Array.init blocks ~f:(fun _ ->
              pipe_with_keep
                ~enable:store_sm.rd_any
                ~n:Store_sm.read_address_pipelining
                store_sm.rd_addr)
        ; input_done = load_sm.done_
        ; output_done = store_sm.done_
        }
    in
    start_input <== cores.start_input;
    start_output <== cores.start_output;
    { O.data_out =
        { Axi_stream.Source.tvalid = store_sm.tvalid
        ; tdata =
            (* We're assuming vivado will do some rejigging here.  We could/should
                instantiate a pipelined mux. *)
            pipe
              ~n:Store_sm.read_data_tree_mux_stages
              ~enable:store_sm.rd_any
              (let qs =
                 List.init blocks ~f:(fun index ->
                   cores.rd_q.(index)
                   |> Array.to_list
                   |> concat_lsb
                   |> pipe_with_keep
                        ~enable:store_sm.rd_any
                        ~n:Store_sm.read_data_pipelining)
               in
               if Config.logblocks = 0
               then List.nth_exn qs 0
               else
                 mux
                   (pipe
                      ~n:
                        (Hardcaml_ntt.Core_config.ram_latency
                        + Store_sm.read_address_pipelining
                        + Store_sm.read_data_pipelining)
                      ~enable:store_sm.rd_any
                      store_sm.block)
                   qs)
        ; tlast = gnd
        ; tkeep = ones (num_cores * Gf.num_bits / 8)
        ; tstrb = ones (num_cores * Gf.num_bits / 8)
        }
    ; data_in_dest = { Axi_stream.Dest.tready = load_sm.tready }
    ; done_ = cores.done_
    }
  ;;

  let hierarchy ~build_mode scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~name:"kernel" ~scope (create ~build_mode)
  ;;
end
