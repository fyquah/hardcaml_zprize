open Base

module T = struct
  type t =
    | SLR0
    | SLR1
    | SLR2
  [@@deriving sexp_of, compare, enumerate]
end

include T
include Comparable.Make (T)
