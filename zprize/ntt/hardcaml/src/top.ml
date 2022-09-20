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
      ; data_in : 'a Axi_stream.Source.t array [@length blocks]
      ; data_out_dest : 'a Axi_stream.Dest.t array [@length blocks]
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:true]
  end

  module O = struct
    type 'a t =
      { data_out : 'a Axi_stream.Source.t array [@length blocks]
      ; data_in_dest : 'a Axi_stream.Dest.t array [@length blocks]
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
      Array.init blocks ~f:(fun index ->
        Load_sm.create
          { Load_sm.I.clock = i.clock
          ; clear = i.clear
          ; tvalid = i.data_in.(index).tvalid
          ; start = start_input
          })
    in
    let store_sm =
      Array.init blocks ~f:(fun index ->
        Store_sm.create
          { Store_sm.I.clock = i.clock
          ; clear = i.clear
          ; tready = i.data_out_dest.(index).tready
          ; start = start_output
          })
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
            Array.init blocks ~f:(fun index ->
              i.data_in.(index).tdata
              |> split_lsb ~part_width:Gf.num_bits
              |> Array.of_list)
        ; wr_en =
            List.init blocks ~f:(fun index ->
              i.data_in.(index).tvalid &: load_sm.(index).tready)
            |> concat_lsb
        ; wr_addr = Array.map load_sm ~f:(fun x -> x.wr_addr)
        ; rd_en = Array.map store_sm ~f:(fun x -> x.rd_en) |> Array.to_list |> concat_lsb
        ; rd_addr = Array.map store_sm ~f:(fun x -> x.rd_addr)
        ; input_done =
            Array.map load_sm ~f:(fun x -> x.done_) |> Array.to_list |> reduce ~f:( &: )
        ; output_done =
            Array.map store_sm ~f:(fun x -> x.done_) |> Array.to_list |> reduce ~f:( &: )
        }
    in
    start_input <== cores.start_input;
    start_output <== cores.start_output;
    { O.data_out =
        Array.init blocks ~f:(fun index ->
          { Axi_stream.Source.tvalid = store_sm.(index).tvalid
          ; tdata = cores.rd_q.(index) |> Array.to_list |> concat_lsb
          ; tlast = gnd
          ; tkeep = ones (num_cores * Gf.num_bits / 8)
          ; tstrb = ones (num_cores * Gf.num_bits / 8)
          })
    ; data_in_dest = Array.map load_sm ~f:(fun x -> { Axi_stream.Dest.tready = x.tready })
    ; done_ = cores.done_
    }
  ;;

  let hierarchy ~build_mode scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~name:"kernel" ~scope (create ~build_mode)
  ;;
end
