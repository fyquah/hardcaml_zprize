(** Computes [(3 * x) mod p] using an a modulo adder and a modulo double. *)

open Hardcaml

val latency : stages:int -> int

val hierarchical
  :  stages:int
  -> p:Z.t
  -> scope:Scope.t
  -> clock:Signal.t
  -> enable:Signal.t
  -> Signal.t
  -> Signal.t
