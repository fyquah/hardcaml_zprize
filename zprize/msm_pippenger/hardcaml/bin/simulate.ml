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
      and verilator = flag "-verilator" no_arg ~doc:"Simulate using Verilator" in
      fun () ->
        let module Config = struct
          include Msm_pippenger.Config.Bls12_377

          let scalar_bits = Option.value scalar_bits_arg ~default:scalar_bits
          let window_size_bits = Option.value window_bits_arg ~default:window_size_bits
        end
        in
        let module Test_kernel = Msm_pippenger_test.Test_kernel_for_vitis.Make (Config) in
        let result = Test_kernel.run_test ~timeout ~verilator num_points in
        if waves
        then
          Hardcaml_waveterm_interactive.run
            ~display_rules:Test_kernel.display_rules
            result.waves]
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
      and verilator = flag "-verilator" no_arg ~doc:" Simulate using Verilator" in
      fun () ->
        let module Config = struct
          include Msm_pippenger.Config.Bls12_377

          let scalar_bits = Option.value scalar_bits_arg ~default:scalar_bits
          let window_size_bits = Option.value window_bits_arg ~default:window_size_bits
        end
        in
        let module Test_top = Msm_pippenger_test.Test_top.Make (Config) in
        let result = Test_top.run_test ~timeout ~verilator num_points in
        if waves
        then
          Hardcaml_waveterm_interactive.run
            ~display_rules:Msm_pippenger_test.Test_top.display_rules
            result.waves]
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"MSM Simulations"
       [ "kernel", command_kernel_for_vitis; "top", command_top ])
;;
