open Core
open Hardcaml
open Field_ops_lib

module Ec_fpn_dbl = Elliptic_curve_lib.Ec_fpn_dbl.With_interface (struct
  let bits = 377
end)

module Ec_fpn_mixed_add = Elliptic_curve_lib.Ec_fpn_mixed_add.With_interface (struct
  let bits = 377
end)

let flag_filename =
  Command.Param.(
    flag
      "output-filename"
      (optional_with_default "/dev/stdout" string)
      ~doc:" Output filename (Defaults to stdout)")
;;

let flag_ground_multiplier =
  let%map_open.Command multiplier_latency =
    flag
      "multiplier-latency"
      (optional_with_default 1 int)
      ~doc:" Latency of ground multipliers"
  and use_vanila_multiply =
    flag
      "use-vanila-multiply"
      no_arg
      ~doc:" Use vanila verilog multplication, rather than optimized hybrid"
  in
  if use_vanila_multiply
  then Ground_multiplier.Config.Verilog_multiply { latency = multiplier_latency }
  else
    Ground_multiplier.Config.Hybrid_dsp_and_luts
      { latency = multiplier_latency; lut_only_hamming_weight_threshold = 10 }
;;

let command_point_double =
  Command.basic
    ~summary:"point double"
    (let%map_open.Command filename = flag_filename in
     fun () ->
       let module M = Ec_fpn_dbl in
       let module C = Circuit.With_interface (M.I) (M.O) in
       let scope = Scope.create ~flatten_design:false () in
       let database = Scope.circuit_database scope in
       let circuit =
         let config =
           Elliptic_curve_lib.Config_presets.For_bls12_377
           .ec_fpn_ops_with_montgomery_reduction
         in
         M.create ~config scope |> C.create_exn ~name:"point_double"
       in
       Rtl.output ~database ~output_mode:(To_file filename) Verilog circuit)
;;

let command_point_add =
  Command.basic
    ~summary:"point add"
    (let%map_open.Command ground_multiplier = flag_ground_multiplier
     and adder_depth =
       flag
         "adder-depth"
         (optional_with_default 3 int)
         ~doc:" Depth of adder stage in montgomery mult. Defaults to 3"
     and subtractor_depth =
       flag
         "subtractor-depth"
         (optional_with_default 3 int)
         ~doc:" Depth of subtractor in montgomery mult. Defaults to 3"
     and filename = flag_filename in
     fun () ->
       let module M = Ec_fpn_mixed_add in
       let module C = Circuit.With_interface (M.I) (M.O) in
       let scope = Scope.create ~flatten_design:false () in
       let database = Scope.circuit_database scope in
       let p = Ark_bls12_377_g1.modulus () in
       let reduce : Elliptic_curve_lib.Ec_fpn_mixed_add.Config.fn =
         let config =
           { Montgomery_reduction.Config.multiplier_config =
               Karatsuba_ofman_mult.Config.generate
                 ~ground_multiplier
                 [ { radix = Radix_2
                   ; pre_adder_stages = 1
                   ; middle_adder_stages = 1
                   ; post_adder_stages = 1
                   }
                 ; { radix = Radix_3
                   ; pre_adder_stages = 1
                   ; middle_adder_stages = 1
                   ; post_adder_stages = 1
                   }
                 ; { radix = Radix_3
                   ; pre_adder_stages = 1
                   ; middle_adder_stages = 1
                   ; post_adder_stages = 1
                   }
                 ]
           ; half_multiplier_config =
               { levels =
                   [ { radix = Radix_2
                     ; pre_adder_stages = 1
                     ; middle_adder_stages = 1
                     ; post_adder_stages = 1
                     }
                   ; { radix = Radix_3
                     ; pre_adder_stages = 1
                     ; middle_adder_stages = 1
                     ; post_adder_stages = 1
                     }
                   ; { radix = Radix_3
                     ; pre_adder_stages = 1
                     ; middle_adder_stages = 1
                     ; post_adder_stages = 1
                     }
                   ]
               ; ground_multiplier
               }
           ; adder_depth
           ; subtractor_depth
           }
         in
         let latency = Montgomery_reduction.Config.latency config in
         let impl ~scope ~clock ~enable x y =
           assert (Option.is_none y);
           Montgomery_reduction.hierarchical ~config ~p ~scope ~clock ~enable x
         in
         { impl; latency }
       in
       let square : Elliptic_curve_lib.Ec_fpn_mixed_add.Config.fn =
         let config =
           { Squarer.Config.levels =
               [ { radix = Radix_2
                 ; pre_adder_stages = 1
                 ; middle_adder_stages = 1
                 ; post_adder_stages = 1
                 }
               ; { radix = Radix_3
                 ; pre_adder_stages = 1
                 ; middle_adder_stages = 1
                 ; post_adder_stages = 1
                 }
               ; { radix = Radix_3
                 ; pre_adder_stages = 1
                 ; middle_adder_stages = 1
                 ; post_adder_stages = 1
                 }
               ]
           ; ground_multiplier
           }
         in
         let latency = Squarer.Config.latency config in
         let impl ~scope ~clock ~enable x y =
           assert (Option.is_none y);
           Squarer.hierarchical ~config ~clock ~enable ~scope x
         in
         { impl; latency }
       in
       let multiply : Elliptic_curve_lib.Ec_fpn_mixed_add.Config.fn =
         let config =
           Karatsuba_ofman_mult.Config.generate
             ~ground_multiplier
             [ { radix = Radix_2
               ; pre_adder_stages = 1
               ; middle_adder_stages = 1
               ; post_adder_stages = 1
               }
             ; { radix = Radix_3
               ; pre_adder_stages = 1
               ; middle_adder_stages = 1
               ; post_adder_stages = 1
               }
             ; { radix = Radix_3
               ; pre_adder_stages = 1
               ; middle_adder_stages = 1
               ; post_adder_stages = 1
               }
             ]
         in
         let latency = Karatsuba_ofman_mult.Config.latency config in
         let impl ~scope ~clock ~enable x y =
           Karatsuba_ofman_mult.hierarchical
             ~enable
             ~config
             ~scope
             ~clock
             x
             (`Signal (Option.value_exn y))
         in
         { latency; impl }
       in
       let circuit =
         M.create ~config:{ multiply; square; reduce; coarse_reduce = reduce; p } scope
         |> C.create_exn ~name:"point_add"
       in
       Rtl.output ~database ~output_mode:(To_file filename) Verilog circuit)
;;

let () =
  [ "point-double", command_point_double; "point-add", command_point_add ]
  |> Command.group ~summary:"generate verilog"
  |> Command_unix.run
;;
