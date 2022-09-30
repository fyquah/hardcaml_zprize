open Hardcaml
open Hardcaml_waveterm

module Make (Config : Msm_pippenger.Config.S) : sig
  val display_rules : Hardcaml_waveterm.Display_rule.t list

  module Utils : module type of Utils.Make (Config)

  type result =
    { waves : Waveform.t option
    ; points : Utils.window_bucket_point list
    ; inputs : Bits.t Utils.Msm_input.t array
    }

  val run_test
    :  ?waves:bool
    -> ?seed:int
    -> ?timeout:int
    -> ?verilator:bool
    -> int
    -> result
end

val test_back_to_back : unit -> Waveform.t
