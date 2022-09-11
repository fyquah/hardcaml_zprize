open! Core

let command_test_pippenger name data =
  ( name
  , Command.basic
      ~summary:("Pippenger test: " ^ name)
      [%map_open.Command
        let waves = flag "-waves" no_arg ~doc:" Show waveform"
        and verbose = flag "-verbose" no_arg ~doc:" Show detailed results" in
        fun () ->
          let waves = Pippenger_test.Test_pippenger.(test ~waves ~verbose data) in
          Option.iter waves ~f:Hardcaml_waveterm_interactive.run] )
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"NTT controller simulation test case"
       Pippenger_test.Test_pippenger.
         [ command_test_pippenger "1-stall" test_1_stall
         ; command_test_pippenger "no-stalls" test_no_stalls
         ; command_test_pippenger "with-stalls" test_with_stalls
         ; command_test_pippenger "fully-stall-window0" test_fully_stall_window0
         ])
;;
