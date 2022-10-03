(* Tests the reference 4 step algorithm. *)

open Core
module Gf = Hardcaml_ntt.Gf.Z
module Reference_model = Hardcaml_ntt.Reference_model.Make (Gf)

let%expect_test "form 2d input matrix, transpose" =
  let input = Array.init 16 ~f:(fun i -> Gf.of_z (Z.of_int i)) in
  let matrix = Reference_model.matrix input 2 2 in
  print_s [%message (matrix : Gf.t array array)];
  let transpose = Reference_model.transpose matrix in
  print_s [%message (transpose : Gf.t array array)];
  [%expect
    {|
    (matrix ((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 13 14 15)))
    (transpose ((0 4 8 12) (1 5 9 13) (2 6 10 14) (3 7 11 15))) |}]
;;

let%expect_test "inverse, 4 step" =
  let input = Array.init 16 ~f:(fun i -> Gf.of_z (Z.of_int i)) in
  let expected = Array.copy input in
  Reference_model.inverse_dit expected;
  print_s [%message (expected : Gf.t array)];
  let four_step = Reference_model.four_step input 2 in
  print_s [%message (four_step : Gf.t array)];
  [%expect
    {|
    (expected
     (120 9185100786013534200 18444501065828136953 9189603281834309625
      18444492269600899065 9185082089752463353 2260596040923128
      9189586793186428920 18446744069414584313 9257157276228155385
      18444483473373661177 9261661979662120952 2251799813685240
      9257140787580274680 2243003586447352 9261643283401050105))
    (four_step
     (120 9185100786013534200 18444501065828136953 9189603281834309625
      18444492269600899065 9185082089752463353 2260596040923128
      9189586793186428920 18446744069414584313 9257157276228155385
      18444483473373661177 9261661979662120952 2251799813685240
      9257140787580274680 2243003586447352 9261643283401050105)) |}]
;;
