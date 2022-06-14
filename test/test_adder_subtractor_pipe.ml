open Core
open Hardcaml_verify
open Comb_gates
open Snarks_r_fun

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
      Adder_subtractor_pipe.For_testing.create_combinational
        (module Comb_gates)
        ~op
        ~stages
        inputs
    in
    let simple =
      List.reduce_exn
        inputs
        ~f:
          (match op with
          | `Add -> ( +: )
          | `Sub -> ( -: ))
    in
    simple ==: optimized.result
  in
  match Solver.solve (cnf ~:eqn) with
  | Ok Unsat -> Ok ()
  | Ok (Sat _) -> Or_error.error_s [%message "Found a counter example!"]
  | Error e -> Or_error.error_s [%message "Error running solver!" (e : Error.t)]
;;
