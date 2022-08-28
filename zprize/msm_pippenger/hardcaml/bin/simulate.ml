open Core

let () =
  Random.set_state (Random.State.make [| 9; 8; 7; 6 |]);
  let waves = Msm_pippenger_test.Test_top.waveform () in
  Hardcaml_waveterm_interactive.run waves
;;

let command_top =
  Command.basic
    ~summary:"Simulate top level msm"
    [%map_open.Command
      let _ = return () in
      fun () ->
        let waves = Msm_pippenger_test.Test_top.waveform () in
        Hardcaml_waveterm_interactive.run waves]
;;

let () =
  Command_unix.run (Command.group ~summary:"MSM Simulations" [ "top", command_top ])
;;
