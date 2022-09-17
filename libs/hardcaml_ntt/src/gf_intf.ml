module type S = sig
  type t [@@deriving sexp_of, compare, equal]

  (** {2 Constants} *)

  (** Solinas prime [2^64 - 2^31 + 1] *)
  val modulus : t

  val zero : t
  val one : t
  val two : t
  val of_z : Z.t -> t
  val to_z : t -> Z.t

  (** {2 Arithmetic operations} *)

  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val negate : t -> t
end
