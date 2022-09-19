open Base
open Hardcaml
open Signal

module Make (Config : Four_step_config.S) = struct
  include Config

  let () =
    if Config.logn < 4
    then
      raise_s
        [%message
          "Minimum logn for 4step algorithm is 4 (256 point total)" (Config.logn : int)]
  ;;

  let cores = 1 lsl logcores

  module Gf = Gf.Signal
  module Single_core = Single_core.With_rams (Config)
  module Controller = Four_step_controller.Make (Config)

  module Axi_stream = Hardcaml_axi.Stream.Make (struct
    let addr_bits = 32
    let data_bits = Gf.num_bits * cores
  end)

  module Parallel_cores = struct
    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; start : 'a
        ; first_4step_pass : 'a
        ; first_iter : 'a
        ; flip : 'a
        ; wr_d : 'a array [@bits Gf.num_bits] [@length cores]
        ; wr_en : 'a [@bits cores]
        ; wr_addr : 'a [@bits logn]
        ; rd_en : 'a [@bits cores]
        ; rd_addr : 'a [@bits logn]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t =
        { done_ : 'a
        ; rd_q : 'a array [@bits Gf.num_bits] [@length cores]
        }
      [@@deriving sexp_of, hardcaml]
    end

    let create ~build_mode scope (i : _ I.t) =
      let cores =
        Array.init cores ~f:(fun index ->
          Single_core.hierarchy
            ~row:index
            ~build_mode
            ~instance:("ntt" ^ Int.to_string index)
            scope
            { Single_core.I.clock = i.clock
            ; clear = i.clear
            ; start = i.start
            ; first_iter = i.first_iter
            ; first_4step_pass = i.first_4step_pass
            ; flip = i.flip
            ; wr_d = i.wr_d.(index)
            ; wr_en = i.wr_en.:(index)
            ; wr_addr = i.wr_addr
            ; rd_en = i.rd_en.:(index)
            ; rd_addr = i.rd_addr
            })
      in
      { O.done_ = cores.(0).done_; rd_q = Array.map cores ~f:(fun core -> core.rd_q) }
    ;;

    let hierarchy ~build_mode scope =
      let module Hier = Hierarchy.In_scope (I) (O) in
      Hier.hierarchical ~name:"parallel_cores" ~scope (create ~build_mode)
    ;;
  end

  module Core = struct
    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; start : 'a
        ; first_4step_pass : 'a
        ; wr_d : 'a array [@bits Gf.num_bits] [@length cores]
        ; wr_en : 'a [@bits cores]
        ; wr_addr : 'a [@bits logn]
        ; rd_en : 'a [@bits cores]
        ; rd_addr : 'a [@bits logn]
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
        ; rd_q : 'a array [@bits Gf.num_bits] [@length cores]
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
        Parallel_cores.hierarchy
          ~build_mode
          scope
          { Parallel_cores.I.clock = i.clock
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
end
