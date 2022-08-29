open Core
open Hardcaml
open Twisted_edwards_lib

module Mixed_add377 = Twisted_edwards_lib.Mixed_add.Make (struct
  let num_bits = 377
end)

let flag_filename =
  Command.Param.(
    flag
      "output-filename"
      (optional_with_default "/dev/stdout" string)
      ~doc:" Output filename (Defaults to stdout)")
;;

let command_mixed_add_barrett_reduction =
  Command.basic
    ~summary:""
    (let%map_open.Command filename = flag_filename in
     fun () ->
       let module C = Circuit.With_interface (Mixed_add377.I) (Mixed_add377.O) in
       let config = Lazy.force Mixed_add.Config.For_bls12_377.with_barrett_reduction in
       let scope = Scope.create () in
       let create = Mixed_add377.create ~config scope in
       let circuit = C.create_exn ~name:"twisted_edwards_mixed_add" create in
       let database = Scope.circuit_database scope in
       Rtl.output ~database ~output_mode:(To_file filename) Verilog circuit)
;;

let () =
  [ "twisted-edwards-mixed-add", command_mixed_add_barrett_reduction ]
  |> Command.group ~summary:"generate verilog for twisted edwards curve operations"
  |> Command_unix.run
;;
