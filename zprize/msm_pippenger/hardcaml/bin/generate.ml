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
          Circuit.create_exn ~name:"ntt" (Top.hierarchical ~build_mode:Synthesis scope)
        in
        Rtl.print ~database:(Scope.circuit_database scope) Verilog circ]
;;

let () = Command_unix.run command_top
