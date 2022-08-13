open! Core

let () =
  let waves = Ntts_r_fun_test.Test_kernel.run () in
  Hardcaml_waveterm_interactive.run waves
;;
