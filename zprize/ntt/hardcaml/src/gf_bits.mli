open! Base
open! Hardcaml

module Make (Bits : Comb.S) : sig
  include Gf_intf.S

  (** {2 Misc} *)

  val num_bits : int
  val is_normalized : t -> Bits.t
  val mul : ?pipe:(t -> t) -> t -> t -> t
  val to_bits : t -> Bits.t
  val of_bits : Bits.t -> t
  val of_z : Z.t -> t
  val to_z : t -> Z.t
end
