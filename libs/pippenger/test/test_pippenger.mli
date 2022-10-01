open! Core
open Hardcaml

module type Config = sig
  include Pippenger.Config.S

  val datapath_depth : int
end

module Msm_input : sig
  type 'a t =
    { scalar : 'a array
    ; affine_point : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module Test (Config : Config) (Scalar_config : Pippenger.Scalar.Scalar_config.S) : sig
  val random_inputs : int -> Bits.t Msm_input.t array
  val debug_inputs : int -> Bits.t Msm_input.t array

  val test
    :  ?waves:bool
    -> ?verbose:bool
    -> ?auto_label_hierarchical_ports:bool
    -> ?can_stall:bool
    -> Bits.t Msm_input.t array
    -> Hardcaml_waveterm.Waveform.t option
end

val test_with_stalls : Bits.t Msm_input.t array
val test_with_twenty_ones : Bits.t Msm_input.t array
val test_no_stalls : Bits.t Msm_input.t array
val test_1_stall : Bits.t Msm_input.t array
val test_fully_stall_window0 : Bits.t Msm_input.t array

val test
  :  ?waves:bool
  -> ?verbose:bool
  -> ?auto_label_hierarchical_ports:bool
  -> ?can_stall:bool
  -> Bits.t Msm_input.t array
  -> Hardcaml_waveterm.Waveform.t option
