open! Base
open! Hardcaml

module Make (Config : Config.S) = struct
  open Config
  open Signal

  module I = struct
    type 'a t =
      { clock : 'a
      ; scalar : 'a [@bits window_size_bits]
      ; bubble : 'a
      ; shift : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { is_in_pipeline : 'a
      ; scalar_out : 'a [@bits window_size_bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let build_pipe ( -- ) ~depth spec shift scalar =
    let rec build_pipe n d pipe =
      if n = depth
      then pipe
      else (
        let d = reg spec ~enable:shift d -- ("scl$" ^ Int.to_string n) in
        build_pipe (n + 1) d (d :: pipe))
    in
    build_pipe 0 scalar [] |> List.rev |> Array.of_list
  ;;

  let rec get_matches pos pipe =
    if pos >= Array.length pipe - 1
    then []
    else (
      Stdio.print_s [%message (pos : int)];
      pipe.(pos) :: get_matches (pos + num_windows) pipe)
  ;;

  let create scope (i : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let spec_no_clear = Reg_spec.create ~clock:i.clock () in
    let pipe =
      build_pipe
        ( -- )
        ~depth:(pipeline_depth + 1)
        spec_no_clear
        i.shift
        (mux2 i.bubble (zero window_size_bits) i.scalar)
    in
    let is_in_pipeline =
      reduce
        ~f:( |: )
        (List.map (get_matches (num_windows - 1) pipe) ~f:(( ==: ) i.scalar))
    in
    { O.is_in_pipeline; scalar_out = Array.last pipe }
  ;;

  let hierarchy scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~name:"track" ~scope create
  ;;
end
