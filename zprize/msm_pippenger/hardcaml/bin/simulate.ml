open Core

let command_top =
  Command.basic
    ~summary:"Simulate top level msm"
    [%map_open.Command
      let _ = return () in
      fun () ->
        let waves = Msm_pippenger_test.Test_top.waveform () in
        Hardcaml_waveterm_interactive.run
          ~display_rules:Msm_pippenger_test.Test_top.display_rules
          waves]
;;

let () =
  Command_unix.run (Command.group ~summary:"MSM Simulations" [ "top", command_top ])
;;
