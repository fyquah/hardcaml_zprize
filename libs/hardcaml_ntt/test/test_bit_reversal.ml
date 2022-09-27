(* Test bit reversed addressing logic. *)

open! Core
module Util = Hardcaml_ntt.Util

let%expect_test "reserve bits in values" =
  print_s
    [%message
      (Util.reverse 8 0b10000000 : Int.Hex.t)
        (Util.reverse 8 0b00000001 : Int.Hex.t)
        (Util.reverse 8 0b11110000 : Int.Hex.t)];
  [%expect
    {|
    (("Util.reverse 8 0b10000000" 0x1) ("Util.reverse 8 0b00000001" 0x80)
     ("Util.reverse 8 0b11110000" 0xf)) |}]
;;

let%expect_test "bit reversed addressing" =
  let open Hardcaml in
  let x = Array.init 8 ~f:(fun i -> Bits.of_int ~width:3 i) in
  Util.bit_reversed_addressing x;
  print_s [%message (x : Bits.t array)];
  [%expect {| (x (000 100 010 110 001 101 011 111)) |}];
  let x = Array.init 8 ~f:(fun i -> Bits.of_int ~width:3 i |> Bits.reverse) in
  Util.bit_reversed_addressing x;
  print_s [%message (x : Bits.t array)];
  [%expect {| (x (000 001 010 011 100 101 110 111)) |}]
;;
