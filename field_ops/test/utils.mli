open Core
module Point = Elliptic_curve_lib.Point

val a_big_prime : Z.t
val sexp_of_z : Z.t -> Sexp.t
val generate_z : lo_incl:Z.t -> hi_incl:Z.t -> Z.t Quickcheck.Generator.t
val random_z : lo_incl:Z.t -> hi_incl:Z.t -> Z.t
val modulo_multiply : p:Z.t -> Z.t -> Z.t -> Z.t
val modulo_inverse : p:Z.t -> Z.t -> Z.t
val affine_to_jacobian : Z.t -> Ark_bls12_377_g1.affine -> Z.t Point.Jacobian.t
val jacobian_to_affine : Z.t -> Z.t Point.Jacobian.t -> Ark_bls12_377_g1.affine
