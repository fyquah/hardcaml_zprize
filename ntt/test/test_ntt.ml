open! Core
module Ntt = Ntts_r_fun.Ntt.Reference
module Gf = Ntt.Gf
module Util = Ntts_r_fun.Util

let%expect_test "reserve bits in values" =
  print_s
    [%message
      (Util.reverse 8 0b10000000 : Int.Hex.t)
        (Util.reverse 8 0b00000001 : Int.Hex.t)
        (Util.reverse 8 0b11110000 : Int.Hex.t)];
  [%expect {|
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

let of_z z = Gf.of_z (Z.of_string z)

let test_transform input expected f =
  f input;
  let expected = Array.map expected ~f:of_z in
  if not
       ([%compare.equal: Gf.t array]
          (Array.subo input ~len:(Array.length expected))
          expected)
  then print_s [%message (input : Gf.t array) (expected : Gf.t array)]
;;

let test input expected =
  test_transform (Array.copy input) expected Ntt.ntt;
  test_transform (Array.copy input) expected Ntt.dit;
  test_transform (Array.copy input) expected Ntt.dif
;;

let linear n =
  Array.init n ~f:(function
      | 0 -> Gf.one
      | 1 -> Gf.two
      | _ -> Gf.zero)
;;

let%expect_test "8pt linear" =
  test
    (linear 8)
    [| "0x0000000000000003"
     ; "0xfffffffefe000002"
     ; "0x0002000000000001"
     ; "0xfffffdff00000202"
     ; "0xffffffff00000000"
     ; "0x0000000002000001"
     ; "0xfffdffff00000002"
     ; "0x000001fffffffe01"
    |];
  [%expect {| |}]
;;

let%expect_test "8pt random" =
  test
    (Array.map
       ~f:of_z
       [| "0xcef967e3e1d0860e"
        ; "0x44be7570bcd4f9df"
        ; "0xf4848ed283e858f2"
        ; "0xa3a3a47eeb6f76f6"
        ; "0xa12d1d0b69c4108b"
        ; "0xeb285d19459ef6c3"
        ; "0x10d812558ad9c103"
        ; "0xd19d3e319d1b6b4a"
       |])
    [| "0x1aaadb56e555836b"
     ; "0x975bcb9d395a282f"
     ; "0x69055db04cf94815"
     ; "0x963cdab11477cc1c"
     ; "0xd05b70dbcf57ddad"
     ; "0xed14bc2fbdc30962"
     ; "0x6c8e69de2cabb133"
     ; "0x9c83c8e1d49cd861"
    |];
  [%expect {| |}]
;;

let%expect_test "4096pt linear" =
  test
    (linear 4096)
    [| "0x0000000000000003"
     ; "0xe586a3342b3bf96c"
     ; "0x0ca769003b43919f"
     ; "0x28b1a9691a680e3c"
     ; "0x3b1e55b017fdb2e4"
     ; "0x309d8a339a00ae6a"
     ; "0xdc13ebf6fd47c483"
     ; "0xc12decfb84bb920e"
    |]
;;

let%expect_test "form matrix, transpose" =
  let input = Array.init 16 ~f:(fun i -> Gf.of_z (Z.of_int i)) in
  let matrix = Ntt.matrix input 2 2 in
  print_s [%message (matrix : Gf.t array array)];
  let transpose = Ntt.transpose matrix in
  print_s [%message (transpose : Gf.t array array)];
  [%expect
    {|
    (matrix ((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 13 14 15)))
    (transpose ((0 4 8 12) (1 5 9 13) (2 6 10 14) (3 7 11 15))) |}]
;;

let%expect_test "4 step" =
  let input = Array.init 16 ~f:(fun i -> Gf.of_z (Z.of_int i)) in
  let expected = Array.copy input in
  Ntt.dit expected;
  print_s [%message (expected : Gf.t array)];
  let four_step = Ntt.four_step input 2 in
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
