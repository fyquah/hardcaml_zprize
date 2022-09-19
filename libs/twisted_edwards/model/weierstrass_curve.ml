open! Base
open Bls12_377_util

type params =
  { a : z
  ; b : z
  ; s : z
  ; alpha : z
  ; twisted_scale : z
  }
[@@deriving sexp_of]

let create_params ~a ~b ~alpha =
  let open Modulo_ops in
  let s = modulo_inverse (modulo_square_root ((of_int 3 * alpha * alpha) + a)) in
  let twisted_scale =
    let frac = ((of_int (-3) * alpha * s) + of_int (-2)) / s in
    modulo_square_root frac
  in
  { a; b; s; alpha; twisted_scale }
;;

type affine =
  { x : z
  ; y : z
  }
[@@deriving sexp_of, equal]
