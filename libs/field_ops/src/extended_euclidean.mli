(** Internal module for computing the extended euclidean algorithm. *)

type t =
  { coef_x : Z.t
  ; coef_y : Z.t
  ; gcd : Z.t
  }

val extended_euclidean : x:Z.t -> y:Z.t -> t
