open Base

type t =
  | SLR0
  | SLR1
  | SLR2
[@@deriving sexp_of, compare, enumerate]

include Comparable.S with type t := t
