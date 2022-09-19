open Core
open Hardcaml
open Msm_pippenger_multi_slr

let command_kernel =
  Command.basic
    ~summary:"Generate MSM pippenger top + kernel wrapper"
    [%map_open.Command
      let window_bits_arg =
        flag
          "-window-bits"
          (optional int)
          ~doc:
            "Override the number of window bits used in the algorithm, to simulate a \
             smaller number of buckets"
      in
      fun () ->
        let module Kernel_for_vitis =
          Kernel_for_vitis.Make (struct
            let t = Config.Bls12_377.t

            let t =
              { t with
                window_size_bits =
                  Option.value window_bits_arg ~default:t.window_size_bits
              }
            ;;
          end)
        in
        let module Circuit =
          Circuit.With_interface (Kernel_for_vitis.I) (Kernel_for_vitis.O)
        in
        let scope = Scope.create ~flatten_design:false () in
        let circ =
          Circuit.create_exn
            ~name:"krnl_msm_pippenger"
            (Kernel_for_vitis.hierarchical ~build_mode:Synthesis scope)
        in
        Rtl.print ~database:(Scope.circuit_database scope) Verilog circ]
;;

let commands = Command.group ~summary:"Generate RTL" [ "kernel", command_kernel ]
let () = Command_unix.run commands
