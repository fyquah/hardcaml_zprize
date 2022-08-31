open Hardcaml
module Config = Twisted_edwards_lib.Mixed_add.Config
module Model = Twisted_edwards_model_lib

module Mixed_add : module type of Twisted_edwards_lib.Mixed_add.Make (struct
  let num_bits = 377
end)

module Xyzt = Mixed_add.Xyzt
module Xyt = Mixed_add.Xyt
module Sim : module type of Cyclesim.With_interface (Mixed_add.I) (Mixed_add.O)

val create_sim : Config.t -> Sim.t

val test
  :  ?debug:bool
  -> config:Config.t
  -> sim:Sim.t
  -> montgomery:bool
  -> (Z.t Xyzt.t * Z.t Xyt.t) list
  -> unit
