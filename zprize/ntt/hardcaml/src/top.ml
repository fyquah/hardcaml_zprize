open Base
open Hardcaml
open Signal

module Make (Config : Hardcaml_ntt.Core_config.S) = struct
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

  let num_cores = cores

  let create ~build_mode scope (i : _ I.t) =
    let start_input = wire 1 in
    let start_output = wire 1 in
    let load_sm =
      Load_sm.hierarchy
        scope
        { Load_sm.I.clock = i.clock
        ; clear = i.clear
        ; tvalid = i.data_in.tvalid
        ; start = start_input
        }
    in
    let store_sm =
      Store_sm.hierarchy
        scope
        { Store_sm.I.clock = i.clock
        ; clear = i.clear
        ; tready = i.data_out_dest.tready
        ; start = start_output
        }
    in
    let pipe = pipeline (Reg_spec.create ~clock:i.clock ()) in
    let cores =
      Four_step.create
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
        ; wr_en = pipe ~n:2 load_sm.wr_en
        ; wr_addr = Array.init blocks ~f:(Fn.const (pipe ~n:2 load_sm.wr_addr))
        ; rd_en = store_sm.rd_en
        ; rd_addr = Array.init blocks ~f:(Fn.const store_sm.rd_addr)
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
              ~enable:(store_sm.first_preroll |: i.data_out_dest.tready)
              ~n:2
              (let qs =
                 List.init blocks ~f:(fun index ->
                   cores.rd_q.(index)
                   |> Array.to_list
                   |> concat_lsb
                   |> pipe ~n:1 ~enable:(store_sm.first_preroll |: i.data_out_dest.tready))
               in
               if Config.logblocks = 0 then List.nth_exn qs 0 else mux store_sm.block qs)
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
