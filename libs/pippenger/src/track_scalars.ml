open! Base
open! Hardcaml

module Make (Config : Config.S) (Scalar_config : Scalar.Scalar_config.S) = struct
  open Config
  open Signal
  module Scalar = Scalar.Make (Scalar_config)

  module I = struct
    type 'a t =
      { clock : 'a
      ; scalar : 'a Scalar.t [@rtlmangle true]
      ; bubble : 'a
      ; shift : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { is_in_pipeline : 'a
      ; scalar_out : 'a Scalar.t [@rtlmangle true]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let build_pipe ( -- ) ~depth spec shift scalar =
    let rec build_pipe n d pipe =
      if n = depth
      then pipe
      else (
        let d =
          Scalar.(
            map ~f:(reg spec ~enable:shift) d
            |> Of_signal.apply_names ~naming_op:(--) ~prefix:("scl$" ^ Int.to_string n))
        in
        build_pipe (n + 1) d (d :: pipe))
    in
    build_pipe 0 scalar [] |> List.rev |> Array.of_list
  ;;

  let scalar_equal (a : _ Scalar.t) (b : _ Scalar.t) = a.scalar ==: b.scalar

  let rec get_matches pos pipe =
    if pos >= Array.length pipe - 1
    then []
    else pipe.(pos) :: get_matches (pos + num_windows) pipe
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
        (Scalar.Of_signal.mux2 i.bubble (Scalar.Of_signal.of_int 0) i.scalar)
    in
    let is_in_pipeline =
      reduce
        ~f:( |: )
        (List.map (get_matches (num_windows - 1) pipe) ~f:(scalar_equal i.scalar))
    in
    { O.is_in_pipeline; scalar_out = Array.last pipe }
  ;;

  let hierarchy scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~name:"track" ~scope create
  ;;
end
