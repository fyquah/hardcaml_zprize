open Core
open Hardcaml_verify
open Comb_gates
open Field_ops_lib

(* Unfortunately, writing proofs for including the carries/borrow in the final
 * comparison would increase the runtime of this proof substantially. But
 * fortunately, the existing computation already make use of the borrows in
 * intermediate stages quite extensively, so this is a sufficient coverage
 * test.
 *)
let test ~op ~bits ~stages ~num_inputs =
  let eqn =
    let inputs = List.init num_inputs ~f:(fun i -> input (sprintf "x%d" i) bits) in
    let optimized =
      Modulo_adder_subtractor_pipe.create
        (module Comb_gates)
        ~stages
        ~pipe:(fun ~n:_ x -> x)
        { lhs = List.hd_exn inputs
        ; rhs_list =
            List.map (List.tl_exn inputs) ~f:(fun term ->
                { Modulo_adder_subtractor_pipe.Term_and_op.term; op })
        }
      |> List.last_exn
      |> Modulo_adder_subtractor_pipe.Single_op_output.result
    in
    let simple =
      List.reduce_exn
        inputs
        ~f:
          (match op with
          | `Add -> ( +: )
          | `Sub -> ( -: ))
    in
    simple ==: optimized
  in
  match Solver.solve (cnf ~:eqn) with
  | Ok Unsat -> Ok ()
  | Ok (Sat _) -> Or_error.error_s [%message "Found a counter example!"]
  | Error e -> Or_error.error_s [%message "Error running solver!" (e : Error.t)]
;;
