open Hardcaml
open Elliptic_curve_lib
module Config = Ec_fpn_dbl.Config
module Jacobian = Point.Jacobian

module Ec_fpn_dbl : module type of Ec_fpn_dbl.With_interface (struct
  let bits = 377
end)

module Sim : module type of Cyclesim.With_interface (Ec_fpn_dbl.I) (Ec_fpn_dbl.O)

val p : Z.t
val create_sim : Config.t -> Sim.t
val modulo_inverse : Z.t -> Z.t
val modulo_multiply : Z.t -> Z.t -> Z.t
val affine_to_jacobian : Ark_bls12_377_g1.affine -> Z.t Jacobian.t

val test
  :  ?debug:bool
  -> config:Config.t
  -> sim:Sim.t
  -> montgomery:bool
  -> Z.t Jacobian.t list
  -> unit
