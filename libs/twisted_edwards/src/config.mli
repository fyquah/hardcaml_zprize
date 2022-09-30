open Core
open Hardcaml
open Signal

module Slr_assignments : sig
  type t =
    { input : int
    ; stage0 : int
    ; stage1a : int
    ; stage1b : int
    ; stage2 : int
    ; stage3 : int
    ; output : int
    }
end

type fn = Elliptic_curve_lib.Ec_fpn_ops_config.fn =
  { latency : int
  ; impl : scope:Scope.t -> clock:Signal.t -> enable:Signal.t -> t -> t option -> t
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
  ; output_pipeline_stages : int
  ; arbitrated_multiplier : bool
  ; slr_assignments : Slr_assignments.t
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
  val with_barrett_reduction_arbitrated : t Lazy.t
  val with_barrett_reduction_full : t Lazy.t
end
