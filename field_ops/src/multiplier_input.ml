open Base
open Hardcaml
open Signal

type t =
  | Multiply_by_constant of (Signal.t * Bits.t)
  | Multiply of (Signal.t * Signal.t)
  | Square of Signal.t

let check_equal ~width_a ~width_b =
  if width_a <> width_b
  then
    raise_s
      [%message
        "Width of [a] and [b] argument of multiplier input mismatch"
          (width_a : int)
          (width_b : int)];
  width_a
;;

let width = function
  | Multiply_by_constant (a, b) -> check_equal ~width_a:(width a) ~width_b:(Bits.width b)
  | Multiply (a, b) -> check_equal ~width_a:(width a) ~width_b:(width b)
  | Square s -> width s
;;
