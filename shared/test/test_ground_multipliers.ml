open Core
open Hardcaml
open Snarks_r_fun.Karatsuba_ofman_mult.For_testing

let generate_z ~width =
  Quickcheck.Generator.map
    ~f:Bigint.to_zarith_bigint
    (Bigint.gen_incl Bigint.zero Bigint.((one lsl width) - one))
;;

type test_case =
  { big : Bits.t
  ; pivot : Bits.t
  }

let test_multiplier_implementation f =
  let generate =
    let%map.Quickcheck.Generator big = generate_z ~width:24
    and pivot = generate_z ~width:7 in
    { big = Bits.of_z ~width:24 big; pivot = Bits.of_z ~width:7 pivot }
  in
  let test_and_print a b =
    let i24 = Bits.of_int ~width:24 in
    let i7 = Bits.of_int ~width:7 in
    let result = Bits.to_int (f { big = i24 a; pivot = i7 b }) in
    Stdio.printf "%d * %d = %d\n\n" a b result
  in
  test_and_print 101 0;
  Quickcheck.test ~trials:100_000 generate ~f:(fun { big; pivot } ->
      let result = f { big; pivot } in
      let expected = Bits.( *: ) big pivot in
      if not (Bits.equal result expected)
      then
        raise_s
          [%message
            "Multiplication result mismatch!"
              (big : Bits.t)
              (pivot : Bits.t)
              (result : Bits.t)
              (expected : Bits.t)])
;;

let%expect_test "" =
  test_multiplier_implementation (fun { big; pivot } ->
      long_multiplication_with_addition (module Bits) ~pivot big);
  [%expect {| 101 * 0 = 0 |}]
;;

let%expect_test "" =
  test_multiplier_implementation (fun { big; pivot } ->
      long_multiplication_with_subtraction (module Bits) ~pivot big);
  [%expect {| 101 * 0 = 0 |}]
;;
