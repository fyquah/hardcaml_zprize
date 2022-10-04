(* Test the Goldilocks field arithmetic. *)

open! Core
open Hardcaml
module Gf = Hardcaml_ntt.Gf

let sexp_of_z z = Z.to_string z |> [%sexp_of: String.t]

let test_vector_z =
  let at_power p i =
    [ Z.((one lsl p) - of_int 10 + of_int i); Z.((one lsl p) + of_int i) ]
  in
  List.init 10 ~f:(fun i ->
    [ [ Z.of_int i ]
    ; at_power 31 i
    ; at_power 32 i
    ; at_power 63 i
    ; [ (let offset = 10 - i in
         Gf.Z.(modulus - Gf.Z.of_z (Z.of_int offset) |> Gf.Z.to_z))
      ]
    ]
    |> List.concat
    |> Array.of_list)
  |> Array.concat
;;

let test_vector = Array.map test_vector_z ~f:Gf.Bits.of_z

let%expect_test "constants" =
  print_s
    [%message
      (Gf.Bits.zero : Gf.Bits.t)
        (Gf.Bits.one : Gf.Bits.t)
        (Gf.Bits.two : Gf.Bits.t)
        (Gf.Bits.modulus : Gf.Bits.t)];
  [%expect
    {|
    ((Gf.Bits.zero 0) (Gf.Bits.one 1) (Gf.Bits.two 2)
     (Gf.Bits.modulus 18446744069414584321)) |}]
;;

let%expect_test "test vectors" =
  print_s [%message (test_vector : Gf.Bits.t array)];
  [%expect
    {|
    (test_vector
     (0 2147483638 2147483648 4294967286 4294967296 9223372036854775798
      9223372036854775808 18446744069414584311 1 2147483639 2147483649 4294967287
      4294967297 9223372036854775799 9223372036854775809 18446744069414584312 2
      2147483640 2147483650 4294967288 4294967298 9223372036854775800
      9223372036854775810 18446744069414584313 3 2147483641 2147483651 4294967289
      4294967299 9223372036854775801 9223372036854775811 18446744069414584314 4
      2147483642 2147483652 4294967290 4294967300 9223372036854775802
      9223372036854775812 18446744069414584315 5 2147483643 2147483653 4294967291
      4294967301 9223372036854775803 9223372036854775813 18446744069414584316 6
      2147483644 2147483654 4294967292 4294967302 9223372036854775804
      9223372036854775814 18446744069414584317 7 2147483645 2147483655 4294967293
      4294967303 9223372036854775805 9223372036854775815 18446744069414584318 8
      2147483646 2147483656 4294967294 4294967304 9223372036854775806
      9223372036854775816 18446744069414584319 9 2147483647 2147483657 4294967295
      4294967305 9223372036854775807 9223372036854775817 18446744069414584320)) |}]
;;

let%expect_test "test vectors are normalized" =
  Array.iter test_vector ~f:(fun x -> assert (Bits.to_bool (Gf.Bits.is_normalized x)))
;;

let%expect_test "compare add implementations" =
  Array.iter test_vector_z ~f:(fun left ->
    Array.iter test_vector_z ~f:(fun right ->
      let actual = Gf.Bits.(of_z left + of_z right |> to_z) in
      let expected = Gf.Z.(of_z left + of_z right |> to_z) in
      if not (Z.equal actual expected)
      then
        raise_s [%message "add failed" (left : z) (right : z) (actual : z) (expected : z)]))
;;

let%expect_test "compare sub implementations" =
  Array.iter test_vector_z ~f:(fun left ->
    Array.iter test_vector_z ~f:(fun right ->
      let actual = Gf.Bits.(of_z left - of_z right |> to_z) in
      let expected = Gf.Z.(of_z left - of_z right |> to_z) in
      if not (Z.equal actual expected)
      then
        raise_s [%message "sub failed" (left : z) (right : z) (actual : z) (expected : z)]))
;;

let%expect_test "compare mul implementations" =
  Array.iter test_vector_z ~f:(fun left ->
    Array.iter test_vector_z ~f:(fun right ->
      let actual = Gf.Bits.(of_z left * of_z right |> to_z) in
      let actual_normalized = Gf.Z.of_z actual in
      let expected = Gf.Z.(of_z left * of_z right |> to_z) in
      if not (Z.equal actual expected)
      then
        raise_s
          [%message
            "mul failed"
              (left : z)
              (right : z)
              (actual : z)
              (actual_normalized : Gf.Z.t)
              (expected : z)]))
;;

let%expect_test "inverse" =
  let test a =
    (* zero is not invertible *)
    if not (Z.equal a Z.zero)
    then (
      let a = Gf.Z.of_z a in
      let inv_a = Gf.Z.inverse a in
      let product = Gf.Z.(a * inv_a) in
      (* a * (1/a) = 1 *)
      if not (Z.equal (Gf.Z.to_z product) Z.one)
      then print_s [%message "failed" (a : Gf.Z.t) (inv_a : Gf.Z.t) (product : Gf.Z.t)])
  in
  Array.iter test_vector_z ~f:test
;;

let%expect_test "powers" =
  let rec loop n x acc =
    if n = 23
    then ()
    else (
      let y = Gf.Z.(acc * x) in
      let z = Gf.Z.pow x n in
      if not (Gf.Z.equal y z)
      then raise_s [%message "bad power" (y : Gf.Z.t) (z : Gf.Z.t) (n : int)];
      loop (n + 1) x y)
  in
  loop 1 (Gf.Z.of_z (Z.of_int 923751)) Gf.Z.one
;;

let%expect_test "roots of unity" =
  let inverse, forward = Hardcaml_ntt.Roots.inverse, Hardcaml_ntt.Roots.forward in
  (* product is [1]. *)
  let prod = Array.map2_exn inverse forward ~f:Gf.Z.( * ) in
  print_s [%message (prod : Gf.Z.t array)];
  [%expect
    {| (prod (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) |}];
  (* compute powers - [w.(i) ^ (2 ^ i) = 1]  *)
  print_s
    [%message
      (Gf.Z.pow inverse.(1) 2 : Gf.Z.t)
        (Gf.Z.pow inverse.(2) 4 : Gf.Z.t)
        (Gf.Z.pow inverse.(3) 8 : Gf.Z.t)
        (Gf.Z.pow inverse.(4) 16 : Gf.Z.t)
        (Gf.Z.pow inverse.(5) 32 : Gf.Z.t)
        (Gf.Z.pow inverse.(6) 64 : Gf.Z.t)];
  [%expect
    {|
    (("Gf.Z.pow (inverse.(1)) 2" 1) ("Gf.Z.pow (inverse.(2)) 4" 1)
     ("Gf.Z.pow (inverse.(3)) 8" 1) ("Gf.Z.pow (inverse.(4)) 16" 1)
     ("Gf.Z.pow (inverse.(5)) 32" 1) ("Gf.Z.pow (inverse.(6)) 64" 1)) |}];
  print_s
    [%message
      (Gf.Z.pow forward.(1) 2 : Gf.Z.t)
        (Gf.Z.pow forward.(2) 4 : Gf.Z.t)
        (Gf.Z.pow forward.(3) 8 : Gf.Z.t)
        (Gf.Z.pow forward.(4) 16 : Gf.Z.t)
        (Gf.Z.pow forward.(5) 32 : Gf.Z.t)
        (Gf.Z.pow forward.(6) 64 : Gf.Z.t)];
  [%expect
    {|
    (("Gf.Z.pow (forward.(1)) 2" 1) ("Gf.Z.pow (forward.(2)) 4" 1)
     ("Gf.Z.pow (forward.(3)) 8" 1) ("Gf.Z.pow (forward.(4)) 16" 1)
     ("Gf.Z.pow (forward.(5)) 32" 1) ("Gf.Z.pow (forward.(6)) 64" 1)) |}]
;;
