open Core
open Hardcaml
open Field_ops_lib.Ground_multiplier.For_testing

let generate_z ~width =
  Quickcheck.Generator.map
    ~f:Bigint.to_zarith_bigint
    (Bigint.gen_incl Bigint.zero Bigint.((one lsl width) - one))
;;

type test_case =
  { big : Bits.t
  ; pivot : Bits.t
  }

let test_24x7 f =
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
  test_24x7 (fun { big; pivot } ->
      long_multiplication_with_addition (module Bits) ~pivot big);
  [%expect {| 101 * 0 = 0 |}]
;;

let test_43x43 f =
  let generate =
    let%map.Quickcheck.Generator a = generate_z ~width:43
    and b = generate_z ~width:43 in
    a, b
  in
  let i43 = Bits.of_int ~width:43 in
  let test_and_print a b =
    let result = Bits.to_z ~signedness:Unsigned (f (i43 a) (i43 b)) in
    Stdio.printf
      "%d * %d = 0x%s %s\n\n"
      a
      b
      (Z.format "x" result)
      (if Z.equal result (Z.( * ) (Z.of_int a) (Z.of_int b))
      then "CORRECT"
      else "INCORRECT")
  in
  test_and_print 101 0;
  test_and_print ((1 lsl 43) - 1) ((1 lsl 43) - 1);
  Quickcheck.test ~trials:100_000 generate ~f:(fun (a, b) ->
      let a = Bits.of_z ~width:43 a in
      let b = Bits.of_z ~width:43 b in
      let result = f a b in
      let expected = Bits.( *: ) a b in
      if not (Bits.equal result expected)
      then
        raise_s
          [%message
            "Multiplication result mismatch!"
              (a : Bits.t)
              (b : Bits.t)
              (result : Bits.t)
              (expected : Bits.t)])
;;

let%expect_test "" =
  test_43x43 (fun a b -> specialized_43_bit_multiply (module Bits) a b);
  [%expect {|
    101 * 0 = 0x0 CORRECT

    8796093022207 * 8796093022207 = 0x3ffffffffff00000000001 CORRECT |}]
;;
