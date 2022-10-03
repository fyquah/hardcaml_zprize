(** Bindings to the g1 modulue in the ark-bls12-377 rust package. This is used
    as a reference implemenetation to test for correctness of our MSM
    implementation.

    Coordinates here are affine coordinates in the regular space (specifically,
    not montgomery space). The underlying rust implementation uses montgomery
    space, this is hidden by this module altogether.
*)

open Core

type affine [@@deriving sexp_of, equal]

(** Constructs an arbitrary value in the finite field. Note that an arbitrarily
    constructed point may not lie on the curve.
*)
val create : x:Z.t -> y:Z.t -> infinity:bool -> affine

(** Performs a point-addition between two points. This operation is
    commutative.
*)
val add : affine -> affine -> affine

(** If non infinity, then return (x, -y). Otherwise, returns infinity. *)
val neg : affine -> affine

(** Given a point p, computes (p * by)

   Raises an exception if [by] is negative.
*)
val mul : affine -> by:int -> affine

(** Returns true if the point is on the eliptic bls12-377 curve. Note that for
    arbitrarily constructed points via [create], this might not be the case.
*)
val is_on_curve : affine -> bool

(** Returns the x-coordinates of the affine point. *)
val x : affine -> Z.t

(** Returns the y-coordinates of the affine point. *)
val y : affine -> Z.t

(** Returns true if the point is in infinity. *)
val infinity : affine -> bool

(** {2 Curve Constants}

   The curve is of the form y^2 = x^3 + ax + b, where y and x are in the finite
   field [modulus]. Note that the values are cached to prevent repeated FFI
   calls into rust.
*)

(** Returns [a] *)
val coeff_a : unit -> Z.t

(** Returns [b] *)
val coeff_b : unit -> Z.t

(** Returns the prime modulus. *)
val modulus : unit -> Z.t

(** Returns the bls12-377 subgroup generator. *)
val subgroup_generator : unit -> affine

(** Multiply by an arbitrary width Hardcaml Bits vector.

    For efficiency [part_width] should be as wide as possible which is 61 bits
    in this implementation.
*)
val mul_wide : part_width:int -> affine -> by:Hardcaml.Bits.t -> affine

module For_testing : sig
  val sexp_of_z : Z.t -> Sexp.t
end
