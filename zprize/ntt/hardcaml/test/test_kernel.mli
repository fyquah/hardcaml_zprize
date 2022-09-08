open! Core

module Make (Config : Ntts_r_fun.Ntt.Config) : sig
  val random_input_coefs : unit -> Z.t list
  val random_input_coef_matrix : unit -> Z.t array array
  val run : ?verbose:bool -> Z.t list -> Hardcaml_waveterm.Waveform.t
  val run2 : ?verbose:bool -> Z.t array array -> Hardcaml_waveterm.Waveform.t
end
