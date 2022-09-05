open! Core

let command_kernel =
  Command.basic
    ~summary:"Simulate kernel operation"
    [%map_open.Command
      let verbose = flag "-verbose" no_arg ~doc:"Print detailed results"
      and input_coefs = flag "-inputs" (optional string) ~doc:"Input coefficients"
      and logn = anon ("LOGN" %: int) in
      fun () ->
        let module Test =
          Ntts_r_fun_test.Test_kernel.Make (struct
            let logn = logn
            let support_4step_twiddle = true
          end)
        in
        let input_coefs =
          match input_coefs with
          | None -> Test.random_input_coefs ()
          | Some filename ->
            let coefs = In_channel.read_lines filename in
            List.map coefs ~f:(fun z -> Z.of_string z)
        in
        let waves = Test.run ~verbose input_coefs in
        Hardcaml_waveterm_interactive.run waves]
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
