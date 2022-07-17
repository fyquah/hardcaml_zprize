open! Core
open Hardcaml
module Gfz = Ntts_r_fun.Gf.Z
module Gf = Ntts_r_fun.Gf.Make (Bits)

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
           Gfz.(modulus - Gfz.of_int offset |> Gfz.to_z))
        ]
      ]
      |> List.concat
      |> Array.of_list)
  |> Array.concat
;;

let test_vector = Array.map test_vector_z ~f:Gf.of_z

let%expect_test "constants" =
  print_s
    [%message
      (Gf.zero : Gf.t)
        (Gf.one : Gf.t)
        (Gf.two : Gf.t)
        (Gf.modulus : Gf.t)
        (Gf.epsilon : Gf.t)
        (Gf.mult_mask : Gf.t)];
  [%expect
    {|
    ((Gf.zero 0) (Gf.one 1) (Gf.two 2) (Gf.modulus 18446744069414584321)
     (Gf.epsilon 4294967295) (Gf.mult_mask 18446744073709551615)) |}]
;;

let%expect_test "test vectors" =
  print_s [%message (test_vector : Gf.t array)];
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
  Array.iter test_vector ~f:(fun x -> assert (Bits.to_bool (Gf.is_normalized x)))
;;

let%expect_test "add" =
  Array.iter test_vector_z ~f:(fun left ->
      Array.iter test_vector_z ~f:(fun right ->
          let actual = Gf.(of_z left +: of_z right |> Gf.to_z) in
          let expected = Gfz.(of_z left + of_z right |> Gfz.to_z) in
          if not (Z.equal actual expected)
          then
            raise_s
              [%message "add failed" (left : z) (right : z) (actual : z) (expected : z)]))
;;

let%expect_test "sub" =
  Array.iter test_vector_z ~f:(fun left ->
      Array.iter test_vector_z ~f:(fun right ->
          let actual = Gf.(of_z left -: of_z right |> Gf.to_z) in
          let expected = Gfz.(of_z left - of_z right |> Gfz.to_z) in
          if not (Z.equal actual expected)
          then
            raise_s
              [%message "sub failed" (left : z) (right : z) (actual : z) (expected : z)]))
;;

let%expect_test "mul" =
  Array.iter test_vector_z ~f:(fun left ->
      Array.iter test_vector_z ~f:(fun right ->
          let actual = Gf.(of_z left *: of_z right |> Gf.to_z) in
          let actual_normalized = Gfz.of_z actual in
          let expected = Gfz.(of_z left * of_z right |> Gfz.to_z) in
          if not (Z.equal actual expected)
          then
            raise_s
              [%message
                "mul failed"
                  (left : z)
                  (right : z)
                  (actual : z)
                  (actual_normalized : Gfz.t)
                  (expected : z)]))
;;

let%expect_test "inverse" =
  let test a =
    (* zero is not invertible *)
    if not (Z.equal a Z.zero)
    then (
      let a = Gfz.of_z a in
      let inv_a = Gfz.inverse a in
      let product = Gfz.(a * inv_a) in
      (* a * (1/a) = 1 *)
      if not (Z.equal (Gfz.to_z product) Z.one)
      then print_s [%message "failed" (a : Gfz.t) (inv_a : Gfz.t) (product : Gfz.t)])
  in
  Array.iter test_vector_z ~f:test
;;
