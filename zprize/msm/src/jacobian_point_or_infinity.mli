open Hardcaml

type 'a t =
  { is_infinity : 'a
  ; point : 'a Jacobian_point.t
  }
[@@deriving sexp_of, hardcaml]

module With_valid : Interface.S with type 'a t = ('a, 'a t) With_valid.t2
