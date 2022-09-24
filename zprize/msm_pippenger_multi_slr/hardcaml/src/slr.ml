open Base

module T = struct
  type t =
    | SLR0
    | SLR1
    | SLR2
  [@@deriving sexp_of, compare, enumerate]

  let to_string x =
    match sexp_of_t x with
    | Atom s -> s
    | _ -> assert false
  ;;
end

include T
include Comparable.Make (T)
