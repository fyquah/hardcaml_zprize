open! Core

let () =
  let waves = Pippenger_test.Test_pippenger.test () in
  Hardcaml_waveterm_interactive.run waves
;;
