open! Core
module Gf : module type of Ntts_r_fun.Gf_bits.Make (Hardcaml.Bits)

val inverse_ntt_test
  :  ?twiddle_4step_config:Ntts_r_fun.Ntt.twiddle_4step_config
  -> ?row:int
  -> ?first_4step_pass:bool
  -> waves:bool
  -> Gf.t array
  -> Hardcaml_waveterm.Waveform.t option * Gf.t array
