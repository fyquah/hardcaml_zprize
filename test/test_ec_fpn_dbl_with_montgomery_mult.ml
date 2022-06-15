open Core
open Hardcaml
open Snarks_r_fun
open Test_ec_fpn_dbl

module Montgomery_mult = Montgomery_mult.With_interface (struct
  let bits = 377
end)

let fp_multiply : Config.fn =
  let multiplier_config = Test_karatsuba_ofman_mult.config_four_stages in
  let montgomery_mult_config =
    { Montgomery_mult.Config.multiplier_config; adder_depth = 3; subtractor_depth = 3 }
  in
  let impl ~scope ~clock ~enable x y =
    assert (Signal.width x = Signal.width y);
    let o =
      Montgomery_mult.create
        ~config:montgomery_mult_config
        ~p
        scope
        { clock; enable; valid = Signal.vdd; x; y }
    in
    o.z
  in
  let latency = Montgomery_mult.Config.latency montgomery_mult_config in
  { impl; latency }
;;

let config = { Config.fp_multiply; p }
let latency = Ec_fpn_dbl.latency config

let%expect_test "latency" =
  Stdio.printf "latency = %d\n" latency;
  [%expect {| latency = 189 |}]
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
