open Hardcaml

(** Returns the latency of a multiplier of the given [depth]. *)
val latency : depth: int -> int

val create
  : ?enable: Signal.t
  -> depth: int
  -> clock:Signal.t
  -> Signal.t
  -> Signal.t
  -> Signal.t
