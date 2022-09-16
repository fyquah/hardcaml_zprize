open Bls12_377_util

type params =
  { c_A : z
  ; c_B : z
  ; twisted_scale : z
  }
[@@deriving sexp_of]

type affine =
  { x : Z.t
  ; y : Z.t
  }
