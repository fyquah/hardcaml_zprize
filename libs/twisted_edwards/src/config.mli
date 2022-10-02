open Core
open Hardcaml
open Signal

module Slr_assignments : sig
  type t =
    { input : int option
    ; stage0a : int option
    ; stage0b : int option
    ; stage1 : int option
    ; stage2 : int option
    ; stage3 : int option
    ; stage4 : int option
    ; stage5 : int option
    ; output : int option
    }
end

type fn = Elliptic_curve_lib.Ec_fpn_ops_config.fn =
  { latency : int
  ; impl : scope:Scope.t -> clock:Signal.t -> enable:Signal.t -> t -> t option -> t
  }

type t =
  { multiply : fn
  ; reduce : fn
  ; coarse_reduce : fn
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

module Reduce : sig
  type t =
    | None
    | Coarse
    | Fine
end

val multiply_latency : reduce:Reduce.t -> t -> int

module For_bls12_377 : sig
  val with_barrett_reduction_arbitrated : t Lazy.t
  val with_barrett_reduction_full : t Lazy.t
end
