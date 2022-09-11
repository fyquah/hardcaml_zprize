open Core

let command_kernel_for_vitis =
  Command.basic
    ~summary:"Simulate top level kernel for Vitis"
    [%map_open.Command
      let _ = return () in
      fun () ->
        let waves = Msm_pippenger_test.Test_kernel_for_vitis.waveform () in
        Hardcaml_waveterm_interactive.run waves]
;;

let command_top_full =
  Command.basic
    ~summary:"Simulate top level msm using Verilator over the full config"
    [%map_open.Command
      let num_points =
        flag "-num-points" (required int) ~doc:"The number of points to simulate over"
      and timeout =
        flag "-timeout" (required int) ~doc:"The number of cycles to use in the timeout"
      in
      fun () ->
        let module Test_top =
          Msm_pippenger_test.Test_top.Make (Msm_pippenger.Config.Bls12_377)
        in
        let _result = Test_top.run_test ~timeout ~verilator:true num_points in
        ()]
;;

let command_top_small =
  Command.basic
    ~summary:"Simulate top level msm over a smaller number of inputs and scalar bits"
    [%map_open.Command
      let _ = return () in
      fun () ->
        let waves = Msm_pippenger_test.Test_top.waveform () in
        Hardcaml_waveterm_interactive.run
          ~display_rules:Msm_pippenger_test.Test_top.display_rules
          waves]
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"MSM Simulations"
       [ "top-small", command_top_small
       ; "kernel", command_kernel_for_vitis
       ; "top-full", command_top_full
       ])
;;
