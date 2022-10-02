open Core
open Hardcaml
open Msm_pippenger

let command_top =
  Command.basic
    ~summary:"Generate MSM pippenger top level"
    [%map_open.Command
      let _ = return () in
      fun () ->
        let module Top = Top.Make (Config.Bls12_377) in
        let module Circuit = Circuit.With_interface (Top.I) (Top.O) in
        let scope = Scope.create ~flatten_design:false () in
        let circ =
          Circuit.create_exn
            ~name:"msm_pippenger_top"
            (Top.hierarchical ~build_mode:Synthesis scope)
        in
        Rtl.print ~database:(Scope.circuit_database scope) Verilog circ]
;;

let command_kernel =
  Command.basic
    ~summary:"Generate MSM pippenger top + kernel wrapper"
    [%map_open.Command
      let scalar_bits_arg =
        flag
          "-scalar-bits"
          (optional int)
          ~doc:
            "Override the number of scalar bits used in the algorithm, to simulate a \
             smaller number of window RAMs"
      and num_windows_arg =
        flag
          "-num-windows"
          (optional int)
          ~doc:
            "Override the number of windows used in the algorithm, to simulate a smaller \
             number of buckets"
      in
      fun () ->
        let module Kernel_for_vitis =
          Kernel_for_vitis.Make (struct
            include Config.Bls12_377

            let scalar_bits = Option.value scalar_bits_arg ~default:scalar_bits
            let num_windows = Option.value num_windows_arg ~default:num_windows
          end)
        in
        let module Circuit =
          Circuit.With_interface (Kernel_for_vitis.I) (Kernel_for_vitis.O)
        in
        let scope =
          Scope.create ~flatten_design:false ~auto_label_hierarchical_ports:true ()
        in
        let circ =
          Circuit.create_exn
            ~name:"krnl_msm_pippenger"
            (Kernel_for_vitis.hierarchical ~build_mode:Synthesis scope)
        in
        printf "//Expecting %i result points\n" Kernel_for_vitis.Top.num_result_points;
        Rtl.print ~database:(Scope.circuit_database scope) Verilog circ]
;;

let command_rtl_checksum =
  Command.basic
    ~summary:
      "Generate a md5 rtl_checksum file for the Kernel_for_vitis BLS12-377 top level. \
       Used to make sure Hardcaml changes do not make unexpected changes in the Verilog."
    [%map_open.Command
      let _ = return () in
      fun () ->
        let module Kernel_for_vitis = Kernel_for_vitis.Make (Config.Bls12_377) in
        let module Circuit =
          Circuit.With_interface (Kernel_for_vitis.I) (Kernel_for_vitis.O)
        in
        let scope =
          Scope.create ~flatten_design:false ~auto_label_hierarchical_ports:true ()
        in
        let circ =
          Circuit.create_exn
            ~name:"krnl_msm_pippenger"
            (Kernel_for_vitis.hierarchical ~build_mode:Synthesis scope)
        in
        let buffer = Buffer.create 2048 in
        Rtl.output
          ~output_mode:(To_buffer buffer)
          ~database:(Scope.circuit_database scope)
          Verilog
          circ;
        let digest = Md5.digest_bytes (Buffer.contents_bytes buffer) in
        print_string (Md5.to_hex digest)]
;;

let commands =
  Command.group
    ~summary:"Generate RTL"
    [ "top", command_top; "kernel", command_kernel; "rtl-checksum", command_rtl_checksum ]
;;

let () = Command_unix.run commands
