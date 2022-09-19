open Core

let command_kernel_for_vitis =
  Command.basic
    ~summary:"Simulate top level kernel for Vitis"
    [%map_open.Command
      let num_points =
        flag "-num-points" (required int) ~doc:" The number of points to simulate over"
      and timeout =
        flag
          "-timeout"
          (optional_with_default 10_000 int)
          ~doc:" The number of cycles to use in the timeout"
      and waves = flag "-waves" no_arg ~doc:" Display waveforms"
      and scalar_bits_arg =
        flag
          "-scalar-bits"
          (optional int)
          ~doc:
            " Override the number of scalar bits used in the algorithm, to simulate a \
             smaller number of window RAMs"
      and window_bits_arg =
        flag
          "-window-bits"
          (optional int)
          ~doc:
            " Override the number of window bits used in the algorithm, to simulate a \
             smaller number of buckets"
      and seed =
        flag
          "-seed"
          (optional_with_default 0 int)
          ~doc:" The seed to use for point generation"
      and verilator = flag "-verilator" no_arg ~doc:"Simulate using Verilator" in
      fun () ->
        let module Config = struct
          include Msm_pippenger.Config.Bls12_377

          let scalar_bits = Option.value scalar_bits_arg ~default:scalar_bits
          let window_size_bits = Option.value window_bits_arg ~default:window_size_bits
        end
        in
<<<<<<< HEAD
        let module Test_kernel = Msm_pippenger_test_top.Test_kernel_for_vitis.Make (Config)
        in
        let result = Test_kernel.run_test ~waves ~seed ~timeout ~verilator num_points in
        Option.iter result.waves ~f:(fun waves ->
          Hardcaml_waveterm_interactive.run ~display_rules:Test_kernel.display_rules waves)]
;;

let command_vitis_kernel_back_to_back =
  Command.basic
    ~summary:"Simulate multiple runs through the vitis kernel"
    [%map_open.Command
      let _ = return () in
      fun () ->
        Hardcaml_waveterm_interactive.run
          (Msm_pippenger_test_top.Test_kernel_for_vitis.test_back_to_back ())]
;;

let command_test_cases =
  Command.group ~summary:"" [ "kernel-back-to-back", command_vitis_kernel_back_to_back ]
=======
        let module Test_kernel = Msm_pippenger_test_top.Test_kernel_for_vitis.Make (Config) in
        let result = Test_kernel.run_test ~timeout ~verilator num_points in
        if waves
        then
          Hardcaml_waveterm_interactive.run
            ~display_rules:Test_kernel.display_rules
            result.waves]
>>>>>>> refs/remotes/origin/ntt-rename
;;

let command_top =
  Command.basic
    ~summary:"Simulate top level MSM"
    [%map_open.Command
      let num_points =
        flag "-num-points" (required int) ~doc:" The number of points to simulate over"
      and timeout =
        flag
          "-timeout"
          (optional_with_default 10_000 int)
          ~doc:" The number of cycles to use in the timeout"
      and waves = flag "-waves" no_arg ~doc:" Display waveforms"
      and scalar_bits_arg =
        flag
          "-scalar-bits"
          (optional int)
          ~doc:
            " Override the number of scalar bits used in the algorithm, to simulate a \
             smaller number of window RAMs"
      and window_bits_arg =
        flag
          "-window-bits"
          (optional int)
          ~doc:
            " Override the number of window bits used in the algorithm, to simulate a \
             smaller number of buckets"
      and verilator = flag "-verilator" no_arg ~doc:" Simulate using Verilator"
      and seed =
        flag
          "-seed"
          (optional_with_default 0 int)
          ~doc:" The seed to use for point generation"
      in
      fun () ->
        let module Config = struct
          include Msm_pippenger.Config.Bls12_377

          let scalar_bits = Option.value scalar_bits_arg ~default:scalar_bits
          let window_size_bits = Option.value window_bits_arg ~default:window_size_bits
        end
        in
        let module Test_top = Msm_pippenger_test_top.Test_top.Make (Config) in
<<<<<<< HEAD
        let result = Test_top.run_test ~seed ~timeout ~verilator num_points in
=======
        let result = Test_top.run_test ~timeout ~verilator num_points in
>>>>>>> refs/remotes/origin/ntt-rename
        if waves
        then
          Hardcaml_waveterm_interactive.run
            ~display_rules:Msm_pippenger_test_top.Test_top.display_rules
            result.waves]
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"MSM Simulations"
       [ "kernel", command_kernel_for_vitis
       ; "top", command_top
       ; "test-cases", command_test_cases
       ])
;;
