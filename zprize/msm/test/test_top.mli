open Hardcaml
open Zprize_msm
module Msm_top : module type of Msm_top.Make (Config.Big)
module Sim : module type of Cyclesim.With_interface (Msm_top.I) (Msm_top.O)

val create_sim : unit -> Sim.t

val test
  :  ?sleep_for_cycles_until_accepting_result:int
  -> ?clear:bool
  -> wiggle_tvalid:bool
  -> batch_size:int
  -> scalar_num_bits:int
  -> scalars:int list
  -> field_points:Ark_bls12_377_g1.affine list
  -> Sim.t
  -> unit
