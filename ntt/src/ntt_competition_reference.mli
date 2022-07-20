open Base

module Make (Gf : Gf_intf.S) : sig
  (** Port of the reference implementation provided to the competition *)
  val ntt : Gf.t array -> unit
end
