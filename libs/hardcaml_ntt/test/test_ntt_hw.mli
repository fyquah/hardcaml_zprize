open! Core
module Gf = Hardcaml_ntt.Gf.Bits

module Test(Config : Hardcaml_ntt.Core_config.S) : sig
  module Single_core : module type of Hardcaml_ntt.Single_core.With_rams(Config)
  module Sim : module type of Hardcaml.Cyclesim.With_interface(Single_core.I)(Single_core.O)

  val inverse_ntt_test_of_sim
    :  ?num_runs:int
    -> ?first_4step_pass:bool
    -> row:int
    -> Sim.t
    -> Gf.t array
    -> Gf.t array
end

val inverse_ntt_test
  :  ?support_4step_twiddle:bool
  -> ?row:int
  -> ?first_4step_pass:bool
  -> ?num_runs:int
  -> waves:bool
  -> Gf.t array
  -> Hardcaml_waveterm.Waveform.t option * Gf.t array
