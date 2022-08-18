open! Core
module Gf : module type of Ntts_r_fun.Gf_bits.Make (Hardcaml.Bits)

val inverse_ntt_test
  :  waves:bool
  -> Gf.t array
  -> Hardcaml_waveterm.Waveform.t option * Gf.t array
