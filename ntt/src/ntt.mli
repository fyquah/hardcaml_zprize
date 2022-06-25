open! Base
module Gf : module type of Gf.Make (Hardcaml.Bits)

val bit_reversed_addressing : 'a array -> unit
val ntt : Gf.t array -> unit
