open Base

type t =
  | SLR0
  | SLR1
  | SLR2
[@@deriving sexp_of, compare, enumerate]

val to_string : t -> string

include Comparable.S with type t := t
