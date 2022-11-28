open! Core

module Make (Config : Zprize_ntt.Top_config.S) : sig
  module Kernel :module type of  Zprize_ntt.For_vitis.Make (Config)
  module Sim : module type of Hardcaml.Cyclesim.With_interface (Kernel.I) (Kernel.O)

  val random_input_coef_matrix : unit -> Z.t array array
  val print_matrix : Hardcaml_ntt.Gf.Z.t array array -> unit
  val copy_matrix : 'a array array -> 'a array array

  val run_with_sim
    :  ?verbose:bool
    -> ?wiggle_prob:float
    -> Sim.t
    -> Hardcaml.Bits.t ref Kernel.I.t
    -> Hardcaml.Bits.t ref Kernel.O.t
    -> Z.t array array
    -> unit

  val run
    :  ?verbose:bool
    -> ?waves:bool
    -> ?wiggle_prob:float
    -> Z.t array array
    -> Hardcaml_waveterm.Waveform.t option
end
