open Core
open Hardcaml

type fn = Snarks_r_fun.Ec_fpn_ops_config.fn =
  { latency : int
  ; impl :
      scope:Scope.t
      -> clock:Signal.t
      -> enable:Signal.t
      -> Signal.t
      -> Signal.t option
      -> Signal.t
  }

type t =
  { multiply : fn
  ; reduce : fn
  ; adder_stages : int
  ; subtractor_stages : int
  ; doubler_stages : int
  ; p : Z.t
  ; a : Z.t
  ; d : Z.t
  }

val reduce
  :  t
  -> scope:Scope.t
  -> clock:Signal.t
  -> enable:Signal.t
  -> Signal.t
  -> Signal.t

val multiply_latency : reduce:bool -> t -> int

module For_bls12_377 : sig
  val with_barrett_reduction : t Lazy.t
end
