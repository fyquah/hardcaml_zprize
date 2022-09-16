(** Port of the reference implementation provided by the competition organisers. *)

open Base

module Make (Gf : Hardcaml_ntt.Gf_intf.S) : sig
  val ntt : Gf.t array -> unit
end
