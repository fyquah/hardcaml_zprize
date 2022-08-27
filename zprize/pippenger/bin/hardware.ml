open! Core

let () =
  Random.set_state (Random.State.make [| 9; 8; 7; 6 |]);
  let waves = Pippenger_test.Test_pippenger.(test test_1_stall) in
  Hardcaml_waveterm_interactive.run waves
;;
