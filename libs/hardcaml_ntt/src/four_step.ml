open Base
open Hardcaml
open Signal

module Make (Config : Core_config.S) = struct
  include Config

  let () =
    if Config.logn < 4
    then
      raise_s
        [%message
          "Minimum logn for 4step algorithm is 4 (256 point total)" (Config.logn : int)]
  ;;

  let cores = 1 lsl logcores
  let blocks = 1 lsl logblocks

  module Gf = Gf.Signal
  module Multi_parallel_cores = Multi_parallel_cores.Make (Config)
  module Controller = Four_step_controller.Make (Config)

  module Axi_stream = Hardcaml_axi.Stream.Make (struct
    let addr_bits = 32
    let data_bits = Gf.num_bits * cores
  end)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; first_4step_pass : 'a
      ; wr_d : 'a Multi_parallel_cores.Q2d.t [@rtlprefix "wr_"]
      ; wr_en : 'a [@bits blocks]
      ; wr_addr : 'a array [@bits logn] [@length blocks]
      ; rd_en : 'a [@bits blocks]
      ; rd_addr : 'a array [@bits logn] [@length blocks]
      ; input_done : 'a
      ; output_done : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { done_ : 'a
      ; start_input : 'a
      ; start_output : 'a
      ; rd_q : 'a Multi_parallel_cores.Q2d.t [@rtlprefix "rd_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create ~build_mode scope (i : _ I.t) =
    let cores_done = wire 1 in
    let controller =
      Controller.hierarchy
        scope
        { Controller.I.clock = i.clock
        ; clear = i.clear
        ; start = i.start
        ; input_done = i.input_done
        ; output_done = i.output_done
        ; cores_done
        }
    in
    let cores =
      Multi_parallel_cores.hierarchy
        ~build_mode
        scope
        { Multi_parallel_cores.I.clock = i.clock
        ; clear = i.clear
        ; start = controller.start_cores
        ; first_iter = controller.first_iter
        ; first_4step_pass = i.first_4step_pass
        ; flip = controller.flip
        ; wr_d = i.wr_d
        ; wr_en = i.wr_en
        ; wr_addr = i.wr_addr
        ; rd_en = i.rd_en
        ; rd_addr = i.rd_addr
        }
    in
    cores_done <== cores.done_;
    { O.done_ = controller.done_
    ; start_input = controller.start_input
    ; start_output = controller.start_output
    ; rd_q = cores.rd_q
    }
  ;;

  let hierarchy ~build_mode scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~name:"cores" ~scope (create ~build_mode)
  ;;
end
