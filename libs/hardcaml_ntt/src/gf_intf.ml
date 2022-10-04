open Base
open Hardcaml

(** Goldilocks field arithmetic. *)
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

module type Gf = sig
  module type S = S

  (** Hardware implementations of the field arithmetic. *)
  module Make (Bits : Comb.S) : sig
    include S

    module Hex : sig
      type nonrec t = t [@@deriving sexp_of]
    end

    (** {2 Misc} *)

    val num_bits : int
    val is_normalized : t -> Bits.t
    val mul : ?pipe:(t -> t) -> t -> t -> t
    val to_bits : t -> Bits.t
    val of_bits : Bits.t -> t
    val of_z : Z.t -> t
    val to_z : t -> Z.t
  end

  (** Big number based reference implementation of the field arithmetic. *)
  module Z : sig
    include S

    (** Display as hex. *)
    module Hex : sig
      type nonrec t = t [@@deriving sexp_of]
    end

    (** {2 Misc} *)

    (** Invert the element *)
    val inverse : t -> t

    (** Raise the element to the given integer power. *)
    val pow : t -> int -> t

    val pp : Formatter.t -> t -> unit

    (** Create a random field element. *)
    val random : unit -> t
  end

  (** Bits based hardware implementation. *)
  module Bits : module type of Make (Bits)

  (** Signal based hardware implementation. *)
  module Signal : module type of Make (Signal)
end
