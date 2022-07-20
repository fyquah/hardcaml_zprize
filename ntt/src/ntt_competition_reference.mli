open Base
module Gf : module type of Gf_bits.Make (Hardcaml.Bits)

(** Implementation of the reference implementation provided to the competition *)
val ntt : Gf.t array -> unit
