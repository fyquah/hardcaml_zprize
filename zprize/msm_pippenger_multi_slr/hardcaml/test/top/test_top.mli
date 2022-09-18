open Hardcaml
open Hardcaml_waveterm
open Msm_pippenger_multi_slr

val display_rules : Hardcaml_waveterm.Display_rule.t list

module Make (C : Pippenger_compute_unit.Pippenger_compute_unit_config.S) : sig
  module Config_for_utils : Config.S
  module Utils : module type of Utils.Make (Config_for_utils)

  type result =
    { waves : Waveform.t
    ; inputs : Bits.t Utils.Msm_input.t array
    ; points : Utils.window_bucket_point list
    }

  val run_test : ?seed:int -> ?timeout:int -> ?verilator:bool -> int -> result
end
