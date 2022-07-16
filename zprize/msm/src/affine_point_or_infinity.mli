open Hardcaml

type 'a t =
  { point : 'a Affine_point.t
  ; is_infinity : 'a
  }
[@@deriving sexp_of, hardcaml]

module With_valid : Interface.S with type 'a t = ('a, 'a t) With_valid.t2
