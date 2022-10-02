open Core
open Hardcaml
open Twisted_edwards_lib

module Num_bits = struct
  let num_bits = 377
end

let flag_filename =
  Command.Param.(
    flag
      "output-filename"
      (optional_with_default "/dev/stdout" string)
      ~doc:" Output filename (Defaults to stdout)")
;;

let command_base_mixed_add_barrett_reduction_arbitrated
  (module Adder : Adder_intf.S)
  ~name
  =
  Command.basic
    ~summary:""
    (let%map_open.Command filename = flag_filename in
     fun () ->
       let module Adder377 = Adder.Make (Num_bits) in
       let module C = Circuit.With_interface (Adder377.I) (Adder377.O) in
       let config = Lazy.force Config.For_bls12_377.with_barrett_reduction_arbitrated in
       let scope = Scope.create () in
       let create = Adder377.create ~config scope in
       let circuit = C.create_exn ~name create in
       let database = Scope.circuit_database scope in
       Rtl.output ~database ~output_mode:(To_file filename) Verilog circuit)
;;

let command_mixed_add_barrett_reduction_arbitrated =
  command_base_mixed_add_barrett_reduction_arbitrated
    (module Twisted_edwards_lib.Mixed_add)
    ~name:"twisted_edwards_mixed_add"
;;

let command_mixed_add_precompute_barrett_reduction_arbitrated =
  command_base_mixed_add_barrett_reduction_arbitrated
    (module Twisted_edwards_lib.Mixed_add_precompute)
    ~name:"twisted_edwards_mixed_add_precompute"
;;

let () =
  [ "twisted-edwards-mixed-add", command_mixed_add_barrett_reduction_arbitrated
  ; ( "twisted-edwards-mixed-add-precompute"
    , command_mixed_add_precompute_barrett_reduction_arbitrated )
  ]
  |> Command.group ~summary:"generate verilog for twisted edwards curve operations"
  |> Command_unix.run
;;
