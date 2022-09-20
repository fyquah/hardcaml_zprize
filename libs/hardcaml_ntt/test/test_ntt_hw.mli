open! Core
module Gf = Hardcaml_ntt.Gf.Bits

val inverse_ntt_test
  :  ?support_4step_twiddle:bool
  -> ?row:int
  -> ?first_4step_pass:bool
  -> ?num_runs:int
  -> waves:bool
  -> Gf.t array
  -> Hardcaml_waveterm.Waveform.t option * Gf.t array
