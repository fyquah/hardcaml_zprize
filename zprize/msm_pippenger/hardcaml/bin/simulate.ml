open Core

let () =
  Random.set_state (Random.State.make [| 9; 8; 7; 6 |]);
  let waves = Msm_pippenger_test.Test_top.waveform () in
  Hardcaml_waveterm_interactive.run waves
;;
