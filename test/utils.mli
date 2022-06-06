open Core

val a_big_prime : Z.t

val sexp_of_z : Z.t -> Sexp.t

val random_z : lo_incl: Z.t -> hi_incl: Z.t -> Z.t
