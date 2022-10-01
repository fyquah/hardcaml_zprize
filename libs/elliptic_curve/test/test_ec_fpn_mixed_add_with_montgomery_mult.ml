open Core
open Elliptic_curve_lib
open Test_ec_fpn_mixed_add

let config = Config_presets.For_bls12_377.ec_fpn_ops_with_montgomery_reduction
let latency = Ec_fpn_mixed_add.latency config

let%expect_test "latency" =
  Stdio.printf "latency = %d\n" latency;
  [%expect {| latency = 283 |}]
;;

let%expect_test "Test on some test cases" =
  Random.init 123;
  test
    ~config
    ~sim:(create_sim config)
    ~montgomery:true
    (List.concat
       [ (* Handcrafted test cases. *)
         [ ( Ark_bls12_377_g1.subgroup_generator ()
           , Ark_bls12_377_g1.mul (Ark_bls12_377_g1.subgroup_generator ()) ~by:2 )
         ]
       ; (* Randomly generated test cases of points added to the generator. *)
         List.init 50 ~f:(fun _ ->
             let gen = Ark_bls12_377_g1.subgroup_generator () in
             let by = Int.max 1 (Random.int Int.max_value) in
             Ark_bls12_377_g1.mul gen ~by, gen)
       ]
    |> List.map ~f:(fun (p0, p1) ->
           ( affine_to_jacobian p0
           , { Point.Affine.x = Ark_bls12_377_g1.x p1; y = Ark_bls12_377_g1.y p1 } )));
  [%expect {| (Ok ()) |}]
;;
