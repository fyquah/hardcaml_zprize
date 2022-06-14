open Hardcaml
open Signal

let latency ~stages =
  Modulo_double_pipe.latency ~stages + Modulo_adder_pipe.latency ~stages
;;

let create ~stages ~p ~scope ~clock ~enable a =
  let spec = Reg_spec.create ~clock () in
  let a_times_2 = Modulo_double_pipe.hierarchical ~enable ~scope ~clock ~stages ~p a in
  let a = pipeline spec ~enable ~n:(Modulo_adder_pipe.latency ~stages) a in
  Modulo_adder_pipe.hierarchical ~scope ~clock ~enable ~stages ~p a a_times_2
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
    let y = create ~stages ~p ~scope ~clock ~enable x in
    { O.y }
  ;;

  let hierarchical ~stages ~p scope (i : _ I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.create
      ~scope
      ~name:(Printf.sprintf "modulo_triple_pipe_%dbits" bits)
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
