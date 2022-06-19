open Base
open Hardcaml
open Signal

type t =
  | Multiply_by_constant of (Signal.t * Bits.t)
  | Multiply of (Signal.t * Signal.t)
  | Square of Signal.t

let width = function
  | Multiply_by_constant _ ->
    (* The input is sanitize to either Multiply or Square. *)
    assert false
  | Multiply (a, b) ->
    if width a <> width b
    then
      raise_s
        [%message
          "Width of [a] and [b] argument of karatsuba ofman multiplier mismatch"
            (width a : int)
            (width b : int)];
    width a
  | Square s -> width s
;;
