open Base
open Hardcaml

type t =
  | Multiply_by_constant of (Signal.t * Bits.t)
  | Multiply of (Signal.t * Signal.t)
  | Square of Signal.t

val width : t -> int
