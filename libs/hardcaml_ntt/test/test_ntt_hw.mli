open! Core
module Gf = Hardcaml_ntt.Gf.Bits

val inverse_ntt_test
  :  ?twiddle_4step_config:Hardcaml_ntt.Core_config.twiddle_4step_config
  -> ?row:int
  -> ?first_4step_pass:bool
  -> ?num_runs:int
  -> waves:bool
  -> Gf.t array
  -> Hardcaml_waveterm.Waveform.t option * Gf.t array
