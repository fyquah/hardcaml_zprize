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
      and waves = flag "-waves" no_arg ~doc:" Display interactive waveform"
      and seed = flag "-seed" (optional_with_default 100 int) ~doc:" Random seed" in
      fun () ->
        let rand_state = Random.State.make [| seed |] in
        Random.set_state rand_state;
        let module Test =
          Ntts_r_fun_test.Test_kernel.Make (struct
            let logn = logn

            let twiddle_4step_config : Ntts_r_fun.Ntt.twiddle_4step_config option =
              Some { rows_per_iteration = 8; log_num_iterations = logn - 3 }
            ;;
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

let command_ntt =
  Command.basic
    ~summary:"Simulate core ntt"
    [%map_open.Command
      let logn = anon ("LOGN" %: int)
      and waves = flag "-waves" no_arg ~doc:"Display waveform" in
      fun () ->
        let input_coefs =
          Array.init (1 lsl logn) ~f:(fun _ ->
            let c = Ntts_r_fun.Gf_z.random () in
            Ntts_r_fun.Gf_z.to_z c |> Ntts_r_fun_test.Test_ntt_hw.Gf.of_z)
        in
        let waves, _result =
          Ntts_r_fun_test.Test_ntt_hw.inverse_ntt_test ~waves input_coefs
        in
        Option.iter waves ~f:Hardcaml_waveterm_interactive.run]
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"NTT Simulations"
       [ "kernel", command_kernel; "ntt", command_ntt ])
;;
