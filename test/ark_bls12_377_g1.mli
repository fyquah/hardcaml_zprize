type affine [@@deriving sexp_of, equal]

val create : x: Z.t -> y: Z.t -> infinity: bool -> affine

val subgroup_generator : unit -> affine

val add : affine -> affine -> affine

val mul : affine -> int -> affine

val is_on_curve : affine -> bool

val x : affine -> Z.t

val y : affine -> Z.t

val infinity : affine -> bool

val coeff_a : unit -> Z.t

val coeff_b : unit -> Z.t
