open! Base
open! Hardcaml
open Signal

module Make (Config : Core_config.S) = struct
  open Config
  module Parallel_cores = Parallel_cores.Make (Config)

  let blocks = 1 lsl logblocks
  let cores = 1 lsl logcores

  module Q2d = struct
    module T = struct
      type 'a t = 'a array array [@@deriving sexp_of]

      let map a ~f = Array.map a ~f:(Array.map ~f)
      let iter a ~f = Array.iter a ~f:(Array.iter ~f)
      let map2 a b ~f = Array.map2_exn a b ~f:(Array.map2_exn ~f)
      let iter2 a b ~f = Array.iter2_exn a b ~f:(Array.iter2_exn ~f)
      let to_list t = Array.map t ~f:Array.to_list |> Array.to_list |> List.concat

      let t =
        Array.init blocks ~f:(fun block ->
          Array.init cores ~f:(fun core ->
            "d_" ^ Int.to_string block ^ "_" ^ Int.to_string core, Gf.Signal.num_bits))
      ;;
    end

    include T
    include Hardcaml.Interface.Make (T)
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; first_4step_pass : 'a
      ; first_iter : 'a
      ; flip : 'a
      ; wr_d : 'a Q2d.t [@rtlprefix "wr_"]
      ; wr_en : 'a [@bits blocks]
      ; wr_addr : 'a array [@bits logn] [@length blocks]
      ; rd_en : 'a [@bits blocks]
      ; rd_addr : 'a array [@bits logn] [@length blocks]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { done_ : 'a
      ; rd_q : 'a Q2d.t [@rtlprefix "rd_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create ~build_mode scope (i : _ I.t) =
    let o =
      Array.init blocks ~f:(fun block ->
        Parallel_cores.hierarchy
          ~single_controller:true
          ~start_row:(block * (1 lsl Config.logcores))
          ~build_mode
          scope
          { Parallel_cores.I.clock = i.clock
          ; clear = i.clear
          ; start = i.start
          ; first_4step_pass = i.first_4step_pass
          ; first_iter = i.first_iter
          ; flip = i.flip
          ; wr_d = i.wr_d.(block)
          ; wr_en = repeat i.wr_en.:(block) cores
          ; wr_addr = i.wr_addr.(block)
          ; rd_en = repeat i.rd_en.:(block) cores
          ; rd_addr = i.rd_addr.(block)
          })
    in
    { O.done_ = o.(0).done_; rd_q = Array.map o ~f:(fun o -> o.rd_q) }
  ;;

  let hierarchy ~build_mode scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~name:"multi_parallel_cores" ~scope (create ~build_mode)
  ;;
end
