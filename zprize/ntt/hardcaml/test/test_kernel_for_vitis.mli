open! Core

module Make (Config : Zprize_ntt.Top_config.S) : sig
  val random_input_coef_matrix : unit -> Z.t array array
  val print_matrix : Hardcaml_ntt.Gf.Z.t array array -> unit
  val copy_matrix : 'a array array -> 'a array array

  val run
    :  ?verbose:bool
    -> ?waves:bool
    -> ?verilator:bool
    -> ?wiggle_prob:float
    -> Z.t array array
    -> Hardcaml_waveterm.Waveform.t option
end
