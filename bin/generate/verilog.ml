open Core
open Hardcaml
open Snarks_r_fun

module Karatsuba_ofman_mult377 = Karatsuba_ofman_mult.With_interface(struct
    let bits = 377
  end)

let flag_filename =
  Command.Param.(
    flag "output-filename"
      (optional_with_default "/dev/stdout" string) ~doc:" Output filename (Defaults to stdout)"
  )
;;

let command_karatsuba_ofman_mult =
  Command.basic ~summary:"karatsuba-ofman-mult"
    (let%map_open.Command filename = flag_filename
     and depth =
       flag "depth" (optional_with_default 4 int)
         ~doc:" Depth karatsuba ofman splitting"
     and multiplier_latency =
       flag "multiplier-latency" (optional_with_default 1 int)
         ~doc:" Latency of ground multipliers"
     and use_vanila_multiply =
       flag "use-vanila-multiply" no_arg
         ~doc:" Use vanila verilog multplication, rather than optimized hybrid"
     in
     fun () ->
       let module K = Karatsuba_ofman_mult377 in
       let module C = Circuit.With_interface(K.I)(K.O) in
       let config =
         Karatsuba_ofman_mult.Config.generate
           ~ground_multiplier:(
             if use_vanila_multiply then
               Verilog_multiply { latency = multiplier_latency }
             else
               Hybrid_dsp_and_luts { latency = multiplier_latency }
           )
           ~depth
       in
       let scope = Scope.create () in
       let circuit =
         C.create_exn ~name:"karatsuba_ofman_mult" (K.create ~config scope)
       in
       Rtl.output ~output_mode:(To_file filename) Verilog circuit)
;;

let () =
  [ "karatsuba-ofman-mult", command_karatsuba_ofman_mult ]
  |> Command.group ~summary:"generate verilog"
  |> Command_unix.run
;;
