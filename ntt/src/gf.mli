open! Base
open! Hardcaml

module Make (Bits : Comb.S) : sig
  type t [@@deriving sexp_of, compare, equal]

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

  (** {2 Misc} *)

  val is_normalized : t -> Bits.t
  val to_bits : t -> Bits.t
  val of_int64 : Int64.t -> t
  val of_z : Z.t -> t
  val to_z : t -> Z.t
end

module Z : sig
  type t [@@deriving sexp_of]

  (** Solinas prime [2^64 - 2^31 + 1] *)
  val modulus : t

  val of_z : Z.t -> t
  val of_int : int -> t
  val to_z : t -> Z.t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
end
