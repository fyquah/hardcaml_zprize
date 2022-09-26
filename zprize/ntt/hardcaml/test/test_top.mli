open! Core
open Hardcaml

module Make (Config : Hardcaml_ntt.Core_config.S) : sig
  open Hardcaml_ntt

  val random_input_coef_matrix : unit -> Z.t array array
  val print_matrix : Gf.Z.t array array -> unit
  val copy_matrix : 'a array array -> 'a array array
  val convert_to_first_pass_input : Gf.Z.t array array -> Gf.Z.t array
  val convert_from_first_pass_output : Gf.Z.t array -> Gf.Z.t array array
  val check_first_pass_output : verbose:bool -> Gf.Z.t array array -> Gf.Z.t array -> unit
  val get_first_pass_results : Bits.t list -> Gf.Z.t array
  val get_second_pass_results : Bits.t list -> Gf.Z.t array array
  val twiddle : Gf.Z.t array array -> unit

  val run
    :  ?verbose:bool
    -> ?waves:bool
    -> first_4step_pass:bool
    -> Z.t array array
    -> Hardcaml_waveterm.Waveform.t option
end
