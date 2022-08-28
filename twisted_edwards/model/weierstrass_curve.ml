open! Base
open Bls12_377_util

type params =
  { a : z
  ; b : z
  ; s : z
  ; alpha : z
  }
[@@deriving sexp_of]

let create_params ~a ~b ~alpha =
  let open Modulo_ops in
  let s = modulo_inverse (modulo_square_root ((of_int 3 * alpha * alpha) + a)) in
  { a; b; s; alpha }
;;

type affine =
  { x : Z.t
  ; y : Z.t
  }