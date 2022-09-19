open! Base
open Hardcaml

val latency : int
val create : clock:Signal.t -> Signal.t -> Signal.t -> Signal.t
