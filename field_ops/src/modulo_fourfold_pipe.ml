open Hardcaml

let latency ~stages = 2 * Modulo_double_pipe.latency ~stages

let create ~stages ~p ~scope ~clock ~enable a =
  a
  |> Modulo_double_pipe.hierarchical ~scope ~clock ~stages ~enable ~p
  |> Modulo_double_pipe.hierarchical ~scope ~clock ~stages ~enable ~p
;;

module With_interface (M : sig
  val bits : int
end) =
struct
  open M

  module I = struct
    type 'a t =
      { clock : 'a
      ; enable : 'a
      ; x : 'a [@bits bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { y : 'a [@bits bits] } [@@deriving sexp_of, hardcaml]
  end

  let create ~stages ~p scope ({ clock; enable; x } : _ I.t) =
    let y = create ~stages ~p ~clock ~enable ~scope x in
    { O.y }
  ;;

  let hierarchical ~stages ~p scope (i : _ I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.create
      ~scope
      ~name:(Printf.sprintf "modulo_fourfold_pipe_%dbits" bits)
      (create ~stages ~p)
      i
  ;;
end

let hierarchical ~stages ~p ~scope ~clock ~enable x =
  let module M =
    With_interface (struct
      let bits = Signal.width x
    end)
  in
  let { M.O.y } = M.hierarchical ~stages ~p scope { clock; enable; x } in
  y
;;
