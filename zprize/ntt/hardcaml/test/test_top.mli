open! Core
open Hardcaml

module Make (Config : Zprize_ntt.Top_config.S) : sig
  open Hardcaml_ntt

  val random_input_coef_matrix : unit -> Z.t array array
  val print_matrix : Gf.Z.t array array -> unit
  val copy_matrix : 'a array array -> 'a array array
  val convert_to_first_pass_input : Gf.Z.t array array -> Gf.Z.t array
  val check_first_pass_output : verbose:bool -> Gf.Z.t array array -> Gf.Z.t array -> unit
  val get_first_pass_results : Bits.t list -> Gf.Z.t array

  val run
    :  ?verbose:bool
    -> ?waves:bool
    -> Z.t array array
    -> Hardcaml_waveterm.Waveform.t option
end
