open! Core

let _sexp_of_z z = Ntts_r_fun.Gf_z.(of_z z |> sexp_of_t)

let command_kernel =
  Command.basic
    ~summary:"Simulate kernel operation"
    [%map_open.Command
      let verbose = flag "-verbose" no_arg ~doc:" Print detailed results"
      and input_coefs = flag "-inputs" (optional string) ~doc:" Input coefficients"
      and logn = anon ("LOGN" %: int)
      and first_4step_pass =
        flag "-first-4step-pass" no_arg ~doc:" Run first pass (outputs are twiddled)"
      and logcores =
        flag
          "-log-cores"
          (optional_with_default 3 int)
          ~doc:" Log number of parallel cores"
      and waves = flag "-waves" no_arg ~doc:" Display interactive waveform"
      and seed = flag "-seed" (optional_with_default 100 int) ~doc:" Random seed" in
      fun () ->
        let rand_state = Random.State.make [| seed |] in
        Random.set_state rand_state;
        let module Test =
          Ntts_r_fun_test.Test_kernel.Make (struct
            let logn = logn

            let twiddle_4step_config : Ntts_r_fun.Ntt.twiddle_4step_config option =
              Some
                { rows_per_iteration = 1 lsl logcores
                ; log_num_iterations = logn - logcores
                }
            ;;

            let logcores = logcores
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
        let waves = Test.run ~verbose ~waves ~first_4step_pass input_coefs in
        Option.iter waves ~f:Hardcaml_waveterm_interactive.run]
;;

let command_kernel_for_vitis =
  Command.basic
    ~summary:"Simulate kernel operation"
    [%map_open.Command
      let verbose = flag "-verbose" no_arg ~doc:" Print detailed results"
      and input_coefs = flag "-inputs" (optional string) ~doc:" Input coefficients"
      and logn = anon ("LOGN" %: int)
      and logcores =
        flag
          "-log-cores"
          (optional_with_default 3 int)
          ~doc:" Log number of parallel cores"
      and waves = flag "-waves" no_arg ~doc:" Display interactive waveform"
      and seed = flag "-seed" (optional_with_default 100 int) ~doc:" Random seed" in
      fun () ->
        let rand_state = Random.State.make [| seed |] in
        Random.set_state rand_state;
        let module Test =
          Ntts_r_fun_test.Test_kernel_for_vitis.Make (struct
            let logn = logn

            let twiddle_4step_config : Ntts_r_fun.Ntt.twiddle_4step_config option =
              Some
                { rows_per_iteration = 1 lsl logcores
                ; log_num_iterations = logn - logcores
                }
            ;;

            let logcores = logcores
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

let command_ntt =
  Command.basic
    ~summary:"Simulate core ntt"
    [%map_open.Command
      let logn = anon ("LOGN" %: int)
      and waves = flag "-waves" no_arg ~doc:" Display waveform"
      and row = flag "-row" (optional int) ~doc:" Row (for twiddle)"
      and rows_per_iteration =
        flag "-rows-per-iteration" (optional int) ~doc:" Rows per iteration (for twiddle)"
      and log_num_iterations =
        flag
          "-log-num-iterations"
          (optional int)
          ~doc:" Log2 num iterations (for twiddle)"
      and first_4step_pass = flag "-first-4step-pass" no_arg ~doc:" Model first pass"
      and seed = flag "-seed" (optional_with_default 100 int) ~doc:" Random seed" in
      fun () ->
        let rand_state = Random.State.make [| seed |] in
        Random.set_state rand_state;
        let input_coefs =
          Array.init (1 lsl logn) ~f:(fun _ ->
            let c = Ntts_r_fun.Gf_z.random () in
            Ntts_r_fun.Gf_z.to_z c |> Ntts_r_fun_test.Test_ntt_hw.Gf.of_z)
        in
        let twiddle_4step_config =
          match rows_per_iteration, log_num_iterations with
          | Some rows_per_iteration, Some log_num_iterations ->
            Some { Ntts_r_fun.Ntt.rows_per_iteration; log_num_iterations }
          | _ -> None
        in
        print_s
          [%message (twiddle_4step_config : Ntts_r_fun.Ntt.twiddle_4step_config option)];
        let waves, _result =
          Ntts_r_fun_test.Test_ntt_hw.inverse_ntt_test
            ?row
            ?twiddle_4step_config
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
       [ "kernel", command_kernel; "vitis", command_kernel_for_vitis; "ntt", command_ntt ])
;;
