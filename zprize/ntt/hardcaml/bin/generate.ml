open! Core
open Hardcaml

let command_kernel =
  Command.basic
    ~summary:"Generate NTT kernel"
    [%map_open.Command
      let logn = anon ("LOGN" %: int)
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
      in
      fun () ->
        let module Kernel_for_vitis =
          Zprize_ntt.For_vitis.Make (struct
            let logn = logn
            let support_4step_twiddle = true
            let logcores = 3
            let logblocks = logblocks
            let memory_layout = memory_layout
          end)
        in
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
    ~summary:"Generate NTT core"
    [%map_open.Command
      let logn = anon ("LOGN" %: int) in
      fun () ->
        let module Ntt =
          Hardcaml_ntt.Single_core.With_rams (struct
            let logn = logn
            let logcores = 0
            let logblocks = 0
            let support_4step_twiddle = false
          end)
        in
        let module Circuit = Circuit.With_interface (Ntt.I) (Ntt.O) in
        let scope = Scope.create ~flatten_design:false () in
        let circ =
          Circuit.create_exn ~name:"ntt" (Ntt.create ~build_mode:Synthesis scope)
        in
        Rtl.print ~database:(Scope.circuit_database scope) Verilog circ]
;;

let command_transposer =
  Command.basic
    ~summary:"Generate transposer core (for synthesis)"
    [%map_open.Command
      let transposer_depth_in_cycles =
        flag "transposer-depth-in-cycles" (required int) ~doc:""
      in
      fun () ->
        let module I = Hardcaml_ntt.Transposer.I in
        let module O = Hardcaml_ntt.Transposer.O in
        let module Circuit = Circuit.With_interface (I) (O) in
        let scope = Scope.create ~flatten_design:false () in
        let circ =
          Circuit.create_exn
            ~name:"transposer"
            (Hardcaml_ntt.Transposer.create ~transposer_depth_in_cycles scope)
        in
        Rtl.print ~database:(Scope.circuit_database scope) Verilog circ]
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"RTL generation"
       [ "kernel", command_kernel; "ntt", command_ntt; "transposer", command_transposer ])
;;
