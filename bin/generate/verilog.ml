open Core
open Hardcaml
open Snarks_r_fun

include struct
  open Snarks_r_fun_test
  module Ark_bls12_377_g1 = Ark_bls12_377_g1
end

module Karatsuba_ofman_mult377 = Karatsuba_ofman_mult.With_interface (struct
  let bits = 377
end)

module Montgometry_mult377 = Montgomery_mult.With_interface (struct
  let bits = 377
end)

module Ec_fpn_dbl = Ec_fpn_dbl.With_interface (struct
  let bits = 377
end)

let flag_filename =
  Command.Param.(
    flag
      "output-filename"
      (optional_with_default "/dev/stdout" string)
      ~doc:" Output filename (Defaults to stdout)")
;;

let flag_depth =
  Command.Param.(
    flag "depth" (optional_with_default 4 int) ~doc:" Depth karatsuba ofman splitting")
;;

let flag_ground_multiplier =
  let%map_open.Command latency =
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
  then Ground_multiplier.Config.Verilog_multiply { latency }
  else Ground_multiplier.Config.Hybrid_dsp_and_luts { latency }
;;

let flag_multiplier_config =
  let%map_open.Command depth = flag_depth
  and ground_multiplier = flag_ground_multiplier in
  Karatsuba_ofman_mult.Config.generate ~ground_multiplier ~depth
;;

let command_karatsuba_ofman_mult =
  Command.basic
    ~summary:"karatsuba-ofman-mult"
    (let%map_open.Command config = flag_multiplier_config
     and filename = flag_filename in
     fun () ->
       let module K = Karatsuba_ofman_mult377 in
       let module C = Circuit.With_interface (K.I) (K.O) in
       let scope = Scope.create () in
       let database = Scope.circuit_database scope in
       let circuit = C.create_exn ~name:"karatsuba_ofman_mult" (K.create ~config scope) in
       Rtl.output ~database ~output_mode:(To_file filename) Verilog circuit)
;;

let command_montgomery_mult =
  Command.basic
    ~summary:"montgomery-mult"
    (let%map_open.Command ground_multiplier = flag_ground_multiplier
     and depth = flag_depth
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
       let module M = Montgometry_mult377 in
       let module C = Circuit.With_interface (M.I) (M.O) in
       let scope = Scope.create ~flatten_design:false () in
       let database = Scope.circuit_database scope in
       let circuit =
         let m0_config = Karatsuba_ofman_mult.Config.generate ~ground_multiplier ~depth in
         let m1_config = { Half_width_multiplier.Config.depth; ground_multiplier } in
         let m2_config = m0_config in
         M.create
           ~config:{ m0_config; m1_config; m2_config; adder_depth; subtractor_depth }
           ~p:(Ark_bls12_377_g1.modulus ())
           scope
         |> C.create_exn ~name:"montgomery_mult"
       in
       Rtl.output ~database ~output_mode:(To_file filename) Verilog circuit)
;;

let command_point_double =
  Command.basic
    ~summary:"point double"
    (let%map_open.Command latency =
       flag "latency" (optional_with_default 1 int) ~doc:" Latency"
     and ground_multiplier = flag_ground_multiplier
     and depth = flag_depth
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
       let module M = Ec_fpn_dbl in
       let module C = Circuit.With_interface (M.I) (M.O) in
       let scope = Scope.create ~flatten_design:false () in
       let database = Scope.circuit_database scope in
       let p = Ark_bls12_377_g1.modulus () in
       let multiplier ~scope ~clock ~enable x y =
         let module M = Montgometry_mult377 in
         let m0_config = Karatsuba_ofman_mult.Config.generate ~ground_multiplier ~depth in
         let m1_config = { Half_width_multiplier.Config.depth; ground_multiplier } in
         let m2_config = m0_config in
         let o =
           M.create
             ~config:{ m0_config; m1_config; m2_config; adder_depth; subtractor_depth }
             ~p
             scope
             { clock; enable; x; y; valid = Signal.vdd }
         in
         o.z
       in
       let circuit =
         M.create ~config:{ fp_multiply = { latency; impl = multiplier }; p } scope
         |> C.create_exn ~name:"point_double"
       in
       Rtl.output ~database ~output_mode:(To_file filename) Verilog circuit)
;;

let () =
  [ "karatsuba-ofman-mult", command_karatsuba_ofman_mult
  ; "montgomery-mult", command_montgomery_mult
  ; "point-dbl", command_point_double
  ]
  |> Command.group ~summary:"generate verilog"
  |> Command_unix.run
;;
