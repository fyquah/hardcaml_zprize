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

let flag_multiplier_config =
  let%map_open.Command depth =
    flag "depth" (optional_with_default 4 int) ~doc:" Depth karatsuba ofman splitting"
  and multiplier_latency =
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
  let full =
    Karatsuba_ofman_mult.Config.generate
      ~ground_multiplier:
        (if use_vanila_multiply
        then Verilog_multiply { latency = multiplier_latency }
        else Hybrid_dsp_and_luts { latency = multiplier_latency })
      ~depth
  in
  let half =
    { Half_width_multiplier.Config.depth
    ; ground_multiplier =
        (if use_vanila_multiply
        then Verilog_multiply { latency = multiplier_latency }
        else Hybrid_dsp_and_luts { latency = multiplier_latency })
    }
  in
  half, full
;;

let command_karatsuba_ofman_mult =
  Command.basic
    ~summary:"karatsuba-ofman-mult"
    (let%map_open.Command _, config = flag_multiplier_config
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
    (let%map_open.Command half_multiplier_config, multiplier_config =
       flag_multiplier_config
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
         M.create
           ~config:
             { multiplier_config
             ; montgomery_reduction_config =
                 { half_multiplier_config
                 ; multiplier_config
                 ; adder_depth
                 ; subtractor_depth
                 }
             }
           ~p:(Ark_bls12_377_g1.modulus ())
           scope
         |> C.create_exn ~name:"montgomery_mult"
       in
       Rtl.output ~database ~output_mode:(To_file filename) Verilog circuit)
;;

let command_point_double =
  Command.basic
    ~summary:"point double"
    (let%map_open.Command half_multiplier_config, multiplier_config =
       flag_multiplier_config
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
       let montgomery_mult_config =
         { Montgomery_mult.Config.multiplier_config
         ; montgomery_reduction_config =
             { half_multiplier_config; multiplier_config; adder_depth; subtractor_depth }
         }
       in
       let multiplier ~scope ~clock ~enable x y =
         let module M = Montgometry_mult377 in
         let o =
           M.create
             ~config:montgomery_mult_config
             ~p
             scope
             { clock; enable; x; y; valid = Signal.vdd }
         in
         o.z
       in
       let circuit =
         let latency = Montgomery_mult.Config.latency montgomery_mult_config in
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
