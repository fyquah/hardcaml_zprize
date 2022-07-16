open Hardcaml

type 'a t =
  { requires_extra_doubling : 'a
  ; point : 'a Jacobian_point_or_infinity.t
  }
[@@deriving sexp_of, hardcaml]

module With_valid : Interface.S with type 'a t = ('a, 'a t) With_valid.t2
