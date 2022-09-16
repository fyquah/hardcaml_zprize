open! Core
module Gf : module type of Zprize_ntt.Gf_bits.Make (Hardcaml.Bits)

val inverse_ntt_test
  :  ?twiddle_4step_config:Zprize_ntt.Ntt.twiddle_4step_config
  -> ?row:int
  -> ?first_4step_pass:bool
  -> ?num_runs:int
  -> waves:bool
  -> Gf.t array
  -> Hardcaml_waveterm.Waveform.t option * Gf.t array
