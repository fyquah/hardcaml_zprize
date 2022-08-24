open! Base
open! Hardcaml

module Make (Config : Config.S) = struct
  open Signal
  open Config

  let pipeline_depth_per_window = (pipeline_depth + num_windows - 1) / num_windows

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; scalar_in : 'a [@bits window_size_bits]
      ; shift : 'a
      ; scalar_match : 'a [@bits window_size_bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { is_in_pipeline : 'a } [@@deriving sexp_of, hardcaml]
  end

  let create _scope (i : _ I.t) =
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let rec build_pipe n d pipe =
      if n = pipeline_depth_per_window
      then pipe
      else (
        let d = reg spec ~enable:i.shift d in
        build_pipe (n + 1) d (d :: pipe))
    in
    let stored = build_pipe 0 i.scalar_in [] in
    { O.is_in_pipeline =
        List.map stored ~f:(fun d -> d ==: i.scalar_match)
        |> tree ~arity:6 ~f:(reduce ~f:( |: ))
    }
  ;;

  let hierarchy ~window scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~name:"bpipe" ~instance:("bp_" ^ Int.to_string window) ~scope create
  ;;
end
