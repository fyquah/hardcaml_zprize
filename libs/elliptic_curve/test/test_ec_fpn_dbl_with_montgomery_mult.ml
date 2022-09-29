open Core
open Elliptic_curve_lib
open Test_ec_fpn_dbl

module Montgomery_mult = Field_ops_lib.Montgomery_mult.With_interface (struct
  let bits = 377
end)

let config = Config_presets.For_bls12_377.ec_fpn_ops_with_montgomery_reduction

let%expect_test "latency" =
  Stdio.printf "latency = %d\n" (Ec_fpn_dbl.latency config);
  [%expect {| latency = 168 |}]
;;

let with_waves = false

let%expect_test "Test on some test cases" =
  Random.init 123;
  let waves, sim =
    let sim = create_sim config in
    if with_waves
    then (
      let waves, sim = Hardcaml_waveterm.Waveform.create sim in
      Some waves, sim)
    else None, sim
  in
  test
    ~config
    ~sim
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
  [%expect {| (Ok ()) |}];
  Option.iter waves ~f:(fun waves ->
      Hardcaml_waveterm.Waveform.Serialize.marshall waves "a.hardcamlwaveform.Z")
;;
