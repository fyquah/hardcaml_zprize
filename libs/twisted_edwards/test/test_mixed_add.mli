open Hardcaml
module Model = Twisted_edwards_model_lib
module Config = Twisted_edwards_lib.Config

module type Test_adder = sig
  module Adder_i : Twisted_edwards_lib.Adder_intf.S

  module Adder : module type of Adder_i.Make (struct
    let num_bits = 377
  end)

  module Xyzt = Adder.Xyzt
  module Xyt = Adder.Xyt
  module Sim : module type of Cyclesim.With_interface (Adder.I) (Adder.O)

  val create_sim : Config.t -> Sim.t

  val test
    :  ?debug:bool
    -> config:Config.t
    -> sim:Sim.t
    -> montgomery:bool
    -> (Z.t Xyzt.t * Z.t Xyt.t * bool) list
    -> unit
end

module Test_mixed_add : Test_adder with module Adder_i := Twisted_edwards_lib.Mixed_add

module Test_mixed_add_precompute :
  Test_adder with module Adder_i := Twisted_edwards_lib.Mixed_add_precompute

module Test_mixed_add_precompute_full :
  Test_adder with module Adder_i := Twisted_edwards_lib.Mixed_add_precompute
