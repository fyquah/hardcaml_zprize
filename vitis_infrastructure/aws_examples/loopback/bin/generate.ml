open Core
open Hardcaml
open Vitis_loopback_example
module Circuit = Circuit.With_interface (I) (O)

let () =
  Command.basic
    ~summary:""
    (let%map_open.Command output = flag "output" (required string) ~doc:"" in
     fun () ->
       let scope = Scope.create () in
       let circuit = Circuit.create_exn ~name:"krnl_loopback" (create scope) in
       let database = Scope.circuit_database scope in
       Rtl.output ~database ~output_mode:(To_file output) Verilog circuit)
  |> Command_unix.run
;;
