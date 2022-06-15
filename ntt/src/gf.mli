open! Base
open! Hardcaml

type t

(** {2 Constants}*)

val zero : t
val one : t
val two : t
val epsilon : t
val modulus : t
val mult_mask : t
val omega : t array

(** {2 Arithmetic operations} *)

val ( +: ) : t -> t -> t
val ( -: ) : t -> t -> t
val negate : t -> t
val ( *: ) : t -> t -> t
