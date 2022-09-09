open! Core
open Hardcaml
open Ntts_r_fun

module Make (Config : Ntts_r_fun.Ntt.Config) : sig
  val random_input_coef_matrix : unit -> Z.t array array
  val print_matrix : Ntts_r_fun.Gf_z.t array array -> unit
  val copy_matrix : 'a array array -> 'a array array
  val get_results : Bits.t list -> Gf_z.t array array

  val run
    :  ?verbose:bool
    -> ?waves:bool
    -> Z.t array array
    -> Hardcaml_waveterm.Waveform.t option
end
