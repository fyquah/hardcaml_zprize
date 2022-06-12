open Core
open Hardcaml_verify
open Comb_gates
open Snarks_r_fun

let test ~bits ~stages ~num_inputs =
  let eqn =
    let inputs = 
      List.init num_inputs ~f:(fun i -> input (sprintf "x%d" i) bits)
    in
    let optimized =
      Adder_pipe.For_testing.create_combinational (module Comb_gates) ~stages inputs
    in
    let simple = List.reduce_exn ~f:(+:) inputs in
    simple ==: optimized.result
  in
  begin match Solver.solve (cnf ~:(eqn)) with
  | Ok Unsat -> Ok ()
  | Ok (Sat _) -> Or_error.error_s [%message "Found a counter example!"]
  | Error e -> Or_error.error_s [%message "Error running solver!" (e : Error.t)]
  end;
;;

let%expect_test "" =
  for num_inputs = 2 to 5 do
    for stages = 1 to 5 do
      let result =
        match test ~bits:32 ~stages:8 ~num_inputs:3 with
        | Ok () -> "OKAY!"
        | Error _ -> "FAILED!"
      in
      Stdio.printf  "num_inputs=%d, stages=%d: %s\n" num_inputs stages result
    done;
  done;
  [%expect {|
    num_inputs=2, stages=1: OKAY!
    num_inputs=2, stages=2: OKAY!
    num_inputs=2, stages=3: OKAY!
    num_inputs=2, stages=4: OKAY!
    num_inputs=2, stages=5: OKAY!
    num_inputs=3, stages=1: OKAY!
    num_inputs=3, stages=2: OKAY!
    num_inputs=3, stages=3: OKAY!
    num_inputs=3, stages=4: OKAY!
    num_inputs=3, stages=5: OKAY!
    num_inputs=4, stages=1: OKAY!
    num_inputs=4, stages=2: OKAY!
    num_inputs=4, stages=3: OKAY!
    num_inputs=4, stages=4: OKAY!
    num_inputs=4, stages=5: OKAY!
    num_inputs=5, stages=1: OKAY!
    num_inputs=5, stages=2: OKAY!
    num_inputs=5, stages=3: OKAY!
    num_inputs=5, stages=4: OKAY!
    num_inputs=5, stages=5: OKAY! |}]
;;
