open! Core
open Hardcaml

module Make (Config : Hardcaml_ntt.Core_config.S) : sig
  val random_input_coef_matrix : unit -> Z.t array array
  val print_matrix : Hardcaml_ntt.Gf.Z.t array array -> unit
  val copy_matrix : 'a array array -> 'a array array
  val get_results : Bits.t list -> Hardcaml_ntt.Gf.Z.t array array
  val twiddle : Hardcaml_ntt.Gf.Z.t array array -> unit

  val run
    :  ?verbose:bool
    -> ?waves:bool
    -> first_4step_pass:bool
    -> Z.t array array
    -> Hardcaml_waveterm.Waveform.t option
end
