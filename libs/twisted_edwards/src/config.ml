open Core
open Hardcaml
open Signal
module Model = Twisted_edwards_model_lib

include struct
  open Elliptic_curve_lib
  module Config_presets = Config_presets
  module Ec_fpn_ops_config = Ec_fpn_ops_config
end

module Slr_assignments = struct
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

type fn = Ec_fpn_ops_config.fn =
  { latency : int
  ; impl : scope:Scope.t -> clock:t -> enable:t -> t -> t option -> t
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

let reduce config ~scope ~clock ~enable x =
  config.reduce.impl ~scope ~clock ~enable x None
;;

let multiply_latency ~reduce (t : t) =
  t.multiply.latency + if reduce then t.reduce.latency else 0
;;

module For_bls12_377 = struct
  let slr_assignments =
    { Slr_assignments.input = 1
    ; stage0 = 1
    ; stage1a = 2
    ; stage1b = 1
    ; stage2 = 2
    ; stage3 = 2
    ; output = 1
    }
  ;;

  let with_barrett_reduction_arbitrated : t Lazy.t =
    let open Config_presets.For_bls12_377 in
    let%map.Lazy { Model.Twisted_edwards_curve.a; d; _ } =
      Model.Bls12_377_params.twisted_edwards
    in
    { multiply
    ; reduce = barrett_reduce
    ; adder_stages = 3
    ; subtractor_stages = 3
    ; doubler_stages = 3
    ; p = Ark_bls12_377_g1.modulus ()
    ; a
    ; d
    ; output_pipeline_stages = 1
    ; arbitrated_multiplier = true
    ; slr_assignments
    }
  ;;

  let with_barrett_reduction_full : t Lazy.t =
    let open Config_presets.For_bls12_377 in
    let%map.Lazy { Model.Twisted_edwards_curve.a; d; _ } =
      Model.Bls12_377_params.twisted_edwards
    in
    { multiply
    ; reduce = barrett_reduce
    ; adder_stages = 3
    ; subtractor_stages = 3
    ; doubler_stages = 3
    ; p = Ark_bls12_377_g1.modulus ()
    ; a
    ; d
    ; output_pipeline_stages = 1
    ; arbitrated_multiplier = false
    ; slr_assignments
    }
  ;;
end
