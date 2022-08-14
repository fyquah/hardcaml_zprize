open! Core

let command_kernel =
  Command.basic
    ~summary:"Simulate kernel operation"
    [%map_open.Command
      let () = return () in
      fun () ->
        let waves = Ntts_r_fun_test.Test_kernel.run ~verbose:true () in
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
