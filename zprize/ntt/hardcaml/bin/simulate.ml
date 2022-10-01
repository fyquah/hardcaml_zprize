open! Core

let command_top =
  Command.basic
    ~summary:"Simulate top operation"
    [%map_open.Command
      let verbose = flag "-verbose" no_arg ~doc:" Print detailed results"
      and input_coefs = flag "-inputs" (optional string) ~doc:" Input coefficients"
      and logn = anon ("LOGN" %: int)
      and logcores =
        flag
          "-log-cores"
          (optional_with_default 3 int)
          ~doc:" Log number of parallel cores"
      and logblocks =
        flag
          "-log-blocks"
          (optional_with_default 0 int)
          ~doc:" Log number of parallel blocks"
      and memory_layout =
        flag
          "-memory-layout"
          (optional_with_default
             Zprize_ntt.Memory_layout.Normal_layout_single_port
             Zprize_ntt.Memory_layout.arg)
          ~doc:" Memory layout"
      and waves = flag "-waves" no_arg ~doc:" Display interactive waveform"
      and seed = flag "-seed" (optional_with_default 100 int) ~doc:" Random seed" in
      fun () ->
        let rand_state = Random.State.make [| seed |] in
        Random.set_state rand_state;
        let module Test =
          Zprize_ntt_test.Test_top.Make (struct
            let logn = logn
            let logcores = logcores
            let logblocks = logblocks
            let support_4step_twiddle = true
            let memory_layout = memory_layout
          end)
        in
        let input_coefs =
          match input_coefs with
          | None -> Test.random_input_coef_matrix ()
          | Some filename ->
            let coefs = In_channel.read_lines filename |> Array.of_list in
            Array.init (1 lsl logn) ~f:(fun row ->
              Array.init (1 lsl logn) ~f:(fun col ->
                Z.of_string coefs.((row * (1 lsl logn)) + col)))
        in
        let waves = Test.run ~verbose ~waves input_coefs in
        Option.iter waves ~f:Hardcaml_waveterm_interactive.run]
;;

let command_kernel_for_vitis =
  Command.basic
    ~summary:"Simulate kernel operation"
    [%map_open.Command
      let verbose = flag "-verbose" no_arg ~doc:" Print detailed results"
      and input_coefs = flag "-inputs" (optional string) ~doc:" Input coefficients"
      and logn = anon ("LOGN" %: int)
      and logblocks =
        flag
          "-log-blocks"
          (optional_with_default 0 int)
          ~doc:" Log number of parallel blocks"
      and memory_layout =
        flag
          "-memory-layout"
          (optional_with_default
             Zprize_ntt.Memory_layout.Normal_layout_single_port
             Zprize_ntt.Memory_layout.arg)
          ~doc:" Memory layout"
      and waves = flag "-waves" no_arg ~doc:" Display interactive waveform"
      and seed = flag "-seed" (optional_with_default 100 int) ~doc:" Random seed"
      and verilator =
        flag
          "verilator"
          no_arg
          ~doc:" Use verilator (rather than the hardcaml simulator) for simulation"
      and wiggle_prob =
        flag "-wiggle-probability-true" (optional float) ~doc:" Wiggle testing"
      in
      fun () ->
        let rand_state = Random.State.make [| seed |] in
        Random.set_state rand_state;
        let module Test =
          Zprize_ntt_test.Test_kernel_for_vitis.Make (struct
            let logn = logn
            let logcores = 3
            let logblocks = logblocks
            let support_4step_twiddle = true
            let memory_layout = memory_layout
          end)
        in
        let input_coefs =
          match input_coefs with
          | None -> Test.random_input_coef_matrix ()
          | Some filename ->
            let coefs = In_channel.read_lines filename |> Array.of_list in
            Array.init (1 lsl logn) ~f:(fun row ->
              Array.init (1 lsl logn) ~f:(fun col ->
                Z.of_string coefs.((row * (1 lsl logn)) + col)))
        in
        let waves = Test.run ~verilator ~verbose ~waves ?wiggle_prob input_coefs in
        Option.iter waves ~f:Hardcaml_waveterm_interactive.run]
;;

let command_ntt =
  Command.basic
    ~summary:"Simulate core ntt"
    [%map_open.Command
      let logn = anon ("LOGN" %: int)
      and waves = flag "-waves" no_arg ~doc:" Display waveform"
      and row = flag "-row" (optional int) ~doc:" Row (for twiddle)"
      and support_4step_twiddle =
        flag "-support-4step-twiddle" no_arg ~doc:" Enable 4step twiddle logic"
      and first_4step_pass = flag "-first-4step-pass" no_arg ~doc:" Model first pass"
      and seed = flag "-seed" (optional_with_default 100 int) ~doc:" Random seed" in
      fun () ->
        let rand_state = Random.State.make [| seed |] in
        Random.set_state rand_state;
        let input_coefs =
          ignore (logn : int);
          [| "0xcef967e3e1d0860e"
           ; "0x44be7570bcd4f9df"
           ; "0xf4848ed283e858f2"
           ; "0xa3a3a47eeb6f76f6"
           ; "0xa12d1d0b69c4108b"
           ; "0xeb285d19459ef6c3"
           ; "0x10d812558ad9c103"
           ; "0xd19d3e319d1b6b4a"
          |]
          |> Array.map ~f:(fun x ->
               x |> Z.of_string |> Hardcaml_ntt_test.Test_ntt_hw.Gf.of_z)
          (*
          Array.init (1 lsl logn) ~f:(fun _ ->
            let c = Hardcaml_ntt.Gf.Z.random () in
            Hardcaml_ntt.Gf.Z.to_z c |> Hardcaml_ntt_test.Test_ntt_hw.Gf.of_z)
          *)
        in
        let waves, _result =
          Hardcaml_ntt_test.Test_ntt_hw.inverse_ntt_test
            ?row
            ~support_4step_twiddle
            ~first_4step_pass
            ~waves
            input_coefs
        in
        Option.iter waves ~f:Hardcaml_waveterm_interactive.run]
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"NTT Simulations"
       [ "top", command_top; "vitis", command_kernel_for_vitis; "ntt", command_ntt ])
;;
