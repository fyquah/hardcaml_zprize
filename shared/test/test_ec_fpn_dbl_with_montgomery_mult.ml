open Core
open Snarks_r_fun
open Test_ec_fpn_dbl

module Montgomery_mult = Montgomery_mult.With_interface (struct
  let bits = 377
end)

let reduce : Snarks_r_fun.Ec_fpn_dbl.Config.fn =
  let config =
    { Montgomery_reduction.Config.multiplier_config =
        Test_karatsuba_ofman_mult.config_four_stages
    ; half_multiplier_config =
        { level_radices = [ Radix_3; Radix_3 ]
        ; ground_multiplier = Specialized_43_bit_multiply
        }
    ; adder_depth = 3
    ; subtractor_depth = 3
    }
  in
  let latency = Montgomery_reduction.Config.latency config in
  let impl ~scope ~clock ~enable x y =
    assert (Option.is_none y);
    Montgomery_reduction.hierarchical ~config ~p ~scope ~clock ~enable x
  in
  { impl; latency }
;;

let square : Snarks_r_fun.Ec_fpn_dbl.Config.fn =
  let config =
    { Squarer.Config.level_radices = [ Radix_2; Radix_3; Radix_3 ]
    ; ground_multiplier = Verilog_multiply { latency = 1 }
    }
  in
  let latency = Squarer.Config.latency config in
  let impl ~scope ~clock ~enable x y =
    assert (Option.is_none y);
    Squarer.hierarchical ~config ~clock ~enable ~scope x
  in
  { impl; latency }
;;

let multiply : Snarks_r_fun.Ec_fpn_dbl.Config.fn =
  let config = Test_karatsuba_ofman_mult.config_four_stages in
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
;;

let config = { Config.multiply; square; reduce; p }
let latency = Ec_fpn_dbl.latency config

let%expect_test "latency" =
  Stdio.printf "latency = %d\n" latency;
  [%expect {| latency = 111 |}]
;;

let%expect_test "Test on some test cases" =
  Random.init 123;
  test
    ~config
    ~sim:(create_sim config)
    ~montgomery:true
    (List.concat
       [ (* Handcrafted test cases. *)
         [ Ark_bls12_377_g1.subgroup_generator ()
         ; Ark_bls12_377_g1.mul (Ark_bls12_377_g1.subgroup_generator ()) ~by:42
         ; Ark_bls12_377_g1.create ~x:(Z.of_int 2) ~y:(Z.of_int 3) ~infinity:false
         ]
       ; (* Randomly generated test csases *)
         List.init 50 ~f:(fun _ ->
             let gen = Ark_bls12_377_g1.subgroup_generator () in
             let by = Int.max 1 (Random.int Int.max_value) in
             Ark_bls12_377_g1.mul gen ~by)
       ]
    |> List.map ~f:affine_to_jacobian);
  [%expect {| (Ok ()) |}]
;;
