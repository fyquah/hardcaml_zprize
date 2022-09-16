open! Core
open Hardcaml
open Zprize_ntt

module Make (Config : Zprize_ntt.Ntt_4step.Config) : sig
  val random_input_coef_matrix : unit -> Z.t array array
  val print_matrix : Zprize_ntt.Gf_z.t array array -> unit
  val copy_matrix : 'a array array -> 'a array array
  val get_results : Bits.t list -> Gf_z.t array array
  val twiddle : Gf_z.t array array -> unit

  val run
    :  ?verbose:bool
    -> ?waves:bool
    -> first_4step_pass:bool
    -> Z.t array array
    -> Hardcaml_waveterm.Waveform.t option
end
