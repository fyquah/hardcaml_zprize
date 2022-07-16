open Hardcaml
open Snarks_r_fun

type 'a t = 'a Point.Jacobian.t =
  { x : 'a
  ; y : 'a
  ; z : 'a
  }
[@@deriving sexp_of, hardcaml]

val of_affine_point : Signal.t Affine_point.t -> Signal.t t

module With_valid : Hardcaml.Interface.S with type 'a t = ('a, 'a t) With_valid.t2
