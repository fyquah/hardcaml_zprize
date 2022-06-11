open Core
open Hardcaml
open Snarks_r_fun

let flag_filename =
  Command.Param.(
    flag "output-filename"
      (optional_with_default "/dev/stdout" string) ~doc:" Output filename"
  )
;;

let command_karatsuba_ofman_mult =
  Command.basic ~summary:"karatsuba-ofman-mult"
    (let%map_open.Command filename = flag_filename in
     fun () ->
       let module K = Karatsuba_ofman_mult.With_interface(struct
           let num_bits = 377
           let depth = 4
         end)
       in
       let module C = Circuit.With_interface(K.I)(K.O) in
       let scope = Scope.create () in
       let circuit =
         C.create_exn ~name:"karatsuba_ofman_mult" (K.create scope)
       in
       Rtl.output ~output_mode:(To_file filename) Verilog circuit)
;;

let () =
  [ "karatsuba-ofman-mult", command_karatsuba_ofman_mult ]
  |> Command.group ~summary:"generate verilog"
  |> Command_unix.run
;;
