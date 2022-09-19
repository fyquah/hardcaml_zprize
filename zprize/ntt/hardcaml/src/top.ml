open Base
open Hardcaml
open Signal

module Make (Config : Hardcaml_ntt.Four_step_config.S) = struct
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
      Load_sm.create
        { Load_sm.I.clock = i.clock
        ; clear = i.clear
        ; tvalid = i.data_in.tvalid
        ; start = start_input
        }
    in
    let store_sm =
      Store_sm.create
        { Store_sm.I.clock = i.clock
        ; clear = i.clear
        ; tready = i.data_out_dest.tready
        ; start = start_output
        }
    in
    let cores =
      Four_step.create
        ~build_mode
        scope
        { Four_step.I.clock = i.clock
        ; clear = i.clear
        ; start = i.start
        ; first_4step_pass = i.first_4step_pass
        ; wr_d =
            (* XX aray: generalize for multiple blocks *)
            [| i.data_in.tdata |> split_lsb ~part_width:Gf.num_bits |> Array.of_list |]
        ; wr_en = repeat (i.data_in.tvalid &: load_sm.tready) blocks
        ; wr_addr = [| load_sm.wr_addr |]
        ; rd_en = repeat store_sm.rd_en blocks
        ; rd_addr = [| store_sm.rd_addr |]
        ; input_done = load_sm.done_
        ; output_done = store_sm.done_
        }
    in
    start_input <== cores.start_input;
    start_output <== cores.start_output;
    { O.data_out =
        { tvalid = store_sm.tvalid
        ; tdata = cores.rd_q.(0) |> Array.to_list |> concat_lsb
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
