open! Core

module Make (Config : Ntts_r_fun.Ntt.Config) : sig
  val random_input_coef_matrix : unit -> Z.t array array

  val run
    :  ?verbose:bool
    -> ?waves:bool
    -> first_4step_pass:bool
    -> Z.t array array
    -> Hardcaml_waveterm.Waveform.t option
end
