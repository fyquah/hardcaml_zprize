open! Base
open! Hardcaml

module Make (Bits : Comb.S) : sig
  type t [@@deriving sexp_of, compare, equal]

  val num_bits : int

  (** {2 Constants} *)

  val zero : t
  val one : t
  val two : t
  val epsilon : t
  val modulus : t

  (** {2 Arithmetic operations} *)

  val ( +: ) : t -> t -> t
  val ( -: ) : t -> t -> t
  val negate : t -> t
  val mul : ?pipe:(t -> t) -> t -> t -> t
  val ( *: ) : t -> t -> t

  (** {2 Misc} *)

  val is_normalized : t -> Bits.t
  val to_bits : t -> Bits.t
  val of_bits : Bits.t -> t
  val of_z : Z.t -> t
  val to_z : t -> Z.t
end
