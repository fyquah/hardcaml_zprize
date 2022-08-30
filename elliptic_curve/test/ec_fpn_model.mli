(** Slow models for eliptic curve operations. *)

open Elliptic_curve_lib

val point_double
  :  montgomery:bool
  -> p:Z.t
  -> Z.t Point.Jacobian.t
  -> Z.t Point.Jacobian.t

val mixed_add
  :  montgomery:bool
  -> p:Z.t
  -> Z.t Point.Affine.t
  -> Z.t Point.Jacobian.t
  -> Z.t Point.Jacobian.t
