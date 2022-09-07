open Core
open Hardcaml
open Field_ops_lib

let%expect_test "Roundtrip to and from NAF" =
  let width = 25 in
  Quickcheck.test
    (Int.gen_incl 0 ((1 lsl width) - 1))
    ~f:(fun x ->
      let y = Bits.to_int (Naf.to_bits (Naf.of_bits (Bits.of_int ~width x))) in
      assert (x = y))
;;

let%expect_test "Demonstrate" =
  Stdio.print_s (Naf.sexp_of_t (Naf.of_bits (Bits.of_bit_string "1110")));
  [%expect {| (Zero Neg_one Zero Zero Pos_one) |}]
;;
