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

type fn = Ec_fpn_ops_config.fn =
  { latency : int
  ; impl : scope:Scope.t -> clock:t -> enable:t -> t -> t option -> t
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

let coarse_reduce config ~scope ~clock ~enable x =
  config.coarse_reduce.impl ~scope ~clock ~enable x None
;;

let reduce config ~scope ~clock ~enable x =
  config.reduce.impl ~scope ~clock ~enable x None
;;

module Reduce = struct
  type t =
    | None
    | Coarse
    | Fine
end

let multiply_latency ~(reduce : Reduce.t) (t : t) =
  let reduce_latency =
    match reduce with
    | None -> 0
    | Coarse -> t.coarse_reduce.latency
    | Fine -> t.reduce.latency
  in
  t.multiply.latency + reduce_latency
;;

module For_bls12_377 = struct
  let slr_assignments =
    (* Only one modulo mult in SLR1, everything else in SLR2 *)
    { Slr_assignments.input = Some 1
    ; stage0a = Some 1
    (** 0a has 2 multiplies *)
    ; stage0b = Some 2
    (** 0b has 1 multiply *)
    ; stage1 = Some 2
    (** Stage1 has addition and subtractions *)
    ; stage2 = Some 2
    (** Stage2 is correction *)
    ; stage3 = Some 2
    (** Stage3 has 4 multiplies *)
    ; stage4 = Some 2
    (** Stage4 has a bunch of addition and subs *)
    ; stage5 = Some 2
    (** Stage5 is correction *)
    ; output = Some 1
    }
  ;;

  let with_barrett_reduction_arbitrated : t Lazy.t =
    let open Config_presets.For_bls12_377 in
    let%map.Lazy { Model.Twisted_edwards_curve.a; d; _ } =
      Model.Bls12_377_params.twisted_edwards
    in
    { multiply
    ; reduce = barrett_reduce
    ; coarse_reduce = barrett_reduce_coarse
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
    ; coarse_reduce = barrett_reduce_coarse
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
