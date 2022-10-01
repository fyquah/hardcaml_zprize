open Hardcaml
open Signal

type fn =
  { latency : int
  ; impl : scope:Scope.t -> clock:t -> enable:t -> t -> t option -> t
  }

type t =
  { multiply : fn
  ; square : fn
  ; reduce : fn
  ; coarse_reduce : fn
  ; p : Z.t
  }

val coarse_reduce
  :  t
  -> scope:Scope.t
  -> clock:Signal.t
  -> enable:Signal.t
  -> Signal.t
  -> Signal.t

val reduce
  :  t
  -> scope:Scope.t
  -> clock:Signal.t
  -> enable:Signal.t
  -> Signal.t
  -> Signal.t

val multiply_latency : reduce:bool -> t -> int
val square_latency : reduce:bool -> t -> int
