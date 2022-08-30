open Core
open Hardcaml
open Signal
module Model = Twisted_edwards_model_lib

type fn = Snarks_r_fun.Ec_fpn_ops_config.fn =
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
  }

let reduce config ~scope ~clock ~enable x =
  config.reduce.impl ~scope ~clock ~enable x None
;;

let multiply_latency ~reduce (t : t) =
  t.multiply.latency + if reduce then t.reduce.latency else 0
;;

module For_bls12_377 = struct
  let with_barrett_reduction : t Lazy.t =
    let open Snarks_r_fun.Config_presets.For_bls12_377 in
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
    }
  ;;
end
