open Base
open Hardcaml
open Signal

module Make (Config : Four_step_config.S) = struct
  open Config

  let cores = 1 lsl logcores

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; first_4step_pass : 'a
      ; first_iter : 'a
      ; flip : 'a
      ; wr_d : 'a array [@bits Gf.Signal.num_bits] [@length cores]
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
      ; rd_q : 'a array [@bits Gf.Signal.num_bits] [@length cores]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Single_core = Single_core.With_rams (Config)

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
