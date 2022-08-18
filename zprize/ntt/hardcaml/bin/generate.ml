open! Core
open Hardcaml

let command_kernel =
  Command.basic
    ~summary:"Generate NTT kernel"
    [%map_open.Command
      let logn = anon ("LOGN" %: int) in
      fun () ->
        let module Ntt_4step =
          Ntts_r_fun.Ntt_4step.Make (struct
            let logn = logn
          end)
        in
        let module Kernel_for_vitis = Ntt_4step.Kernel_for_vitis in
        let module Circuit =
          Circuit.With_interface (Kernel_for_vitis.I) (Kernel_for_vitis.O)
        in
        let scope = Scope.create ~flatten_design:false () in
        let circ =
          Circuit.create_exn
            ~name:"krnl_ntt"
            (Kernel_for_vitis.create ~build_mode:Synthesis scope)
        in
        Rtl.print ~database:(Scope.circuit_database scope) Verilog circ]
;;

let command_ntt =
  Command.basic
    ~summary:"Generate NTT kernel"
    [%map_open.Command
      let logn = anon ("LOGN" %: int) in
      fun () ->
        let module Ntts =
          Ntts_r_fun.Ntt.Make (struct
            let logn = logn
          end)
        in
        let module Ntt = Ntts.With_rams in
        let module Circuit = Circuit.With_interface (Ntt.I) (Ntt.O) in
        let scope = Scope.create ~flatten_design:false () in
        let circ =
          Circuit.create_exn ~name:"ntt" (Ntt.create ~build_mode:Synthesis scope)
        in
        Rtl.print ~database:(Scope.circuit_database scope) Verilog circ]
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"RTL generation"
       [ "kernel", command_kernel; "ntt", command_ntt ])
;;
