open! Core
open Hardcaml

module Msm_input : sig
  type 'a t =
    { scalar : 'a array
    ; affine_point : 'a
    }
  [@@deriving sexp_of, hardcaml]

  val random_inputs : unit -> Bits.t t array
end

val test_with_stalls : Bits.t Msm_input.t array
val test_no_stalls : Bits.t Msm_input.t array
val test_1_stall : Bits.t Msm_input.t array
val test_fully_stall_window0 : Bits.t Msm_input.t array

val test
  :  ?waves:bool
  -> ?verbose:bool
  -> ?auto_label_hierarchical_ports:bool
  -> Bits.t Msm_input.t array
  -> Hardcaml_waveterm.Waveform.t option
