type t = private bytes [@@deriving sexp_of]

val of_affine_points : Ark_bls12_377_g1.affine list -> t
val to_affine_points : t -> Ark_bls12_377_g1.affine list
