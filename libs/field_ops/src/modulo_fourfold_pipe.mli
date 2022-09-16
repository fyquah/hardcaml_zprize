(** Computes [(4 * x) mod p] *)

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
