open Hardcaml
open Hardcaml_waveterm

(** Waveform for a small test. *)
val waveform : unit -> Hardcaml_waveterm.Waveform.t

val display_rules : Hardcaml_waveterm.Display_rule.t list

module Make (Config : Msm_pippenger.Config.S) : sig
  module Utils : module type of Utils.Make (Config)

  type result =
    { waves : Waveform.t
    ; inputs : Bits.t Utils.Msm_input.t array
    ; points : Utils.window_bucket_point list
    }

  val run_test : ?timeout:int -> ?verilator:bool -> int -> result
end
