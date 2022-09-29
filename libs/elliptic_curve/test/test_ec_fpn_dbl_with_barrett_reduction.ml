open Core
open Test_ec_fpn_dbl

let config =
  Elliptic_curve_lib.Config_presets.For_bls12_377.ec_fpn_ops_with_barrett_reduction
;;

let latency = Ec_fpn_dbl.latency config

let%expect_test "latency" =
  Stdio.printf "latency = %d\n" latency;
  [%expect {| latency = 228 |}]
;;

let%expect_test "Test on random test cases" =
  Random.init 456;
  test
    ~config
    ~sim:(create_sim config)
    ~montgomery:false
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

(* The following is useful for debugging a single failing test case.

   {[
     let mod_p a =
       let open Z in
       let a = a mod p in
       if lt a Z.zero then
         a + p
       else
         a
     ;;

     let print_intermediate_values (test_input : Ark_bls12_377_g1.affine) =
       let open Z in
       let sexp_of_z = Utils.sexp_of_z in
       let x = Ark_bls12_377_g1.x test_input in
       let y = Ark_bls12_377_g1.y test_input in
       let z = one in
       let s = mod_p (of_int 4 * x * pow y 2) in
       let m = mod_p (of_int 3 * pow x 2) in
       let x' = mod_p (pow m 2 - (of_int 2 * s)) in
       let s_minus_x' = mod_p (s - x') in
       let m_times_s_minus_x'_before_mod = (m * s_minus_x') in
       let m_times_s_minus_x' = mod_p m_times_s_minus_x'_before_mod in
       let y_pow_4_times_8 = mod_p ((of_int 8) * (pow y 4)) in
       let y' = mod_p (m_times_s_minus_x' - y_pow_4_times_8) in
       let z' = mod_p (of_int 2 * y * z) in
       Stdio.print_s [%message
         (s : z)
           (m : z)
           (x' : z)
           (s_minus_x' : z)
           (y_pow_4_times_8 : z)
           (m_times_s_minus_x'_before_mod : z)
           (m_times_s_minus_x' : z)
           (y' : z)
           (z' : z)
       ]
     ;;

     let%expect_test "Debug" =
       let test_input =
         let gen = Ark_bls12_377_g1.subgroup_generator () in
         let by = Int.max 1 (Random.int Int.max_value) in
         Ark_bls12_377_g1.mul gen ~by
       in
       Stdio.print_s [%message (test_input : Ark_bls12_377_g1.affine)];
       [%expect {|
      (test_input
       ((x
         0x478ba0b1781e73cdb727e1ef76286ce84d63aaf42419bb877722c594deac31d53f9279dba37bbf89ef7bcee4408daa)
        (y
         0x119fb4d6acd97ef618f5bafc7776c8f3128980fde64f5c8e509255d4ea3b268f5f996bb1a5fbf8c1e871c3125ec70b2)
        (infinity false))) |}];
       [%expect {| |}];
       let sim = create_sim config in
       let internal_ports = List.map ~f:fst (Cyclesim.internal_ports sim) in
       Stdio.print_s [%message (internal_ports : string list)];
       [%expect {|
      (internal_ports
       (stage0$z stage1$y_times_z stage2$z' stage3$z' stage4$z' stage5$z' stage6$z'
        stage7$z stage2$y_pow_4 stage3$y_pow_4_times_2 stage4$y_pow_4_times_4
        stage5$y_pow_4_times_8 stage6$y_pow_4_times_8 stage3$s stage4$s
        stage5$s_minus_x' stage3$m stage4$m stage5$m stage6$m_times_s_minus_x'
        stage7$y stage0$y stage1$y_squared stage1$x_times_4 stage2$s
        stage3$s_times_2 stage0$x stage1$x_squared stage2$m stage3$m_pow_2 gnd
        stage4$x' stage5$x' stage6$x' stage7$x vdd stage0$valid stage1$valid
        stage2$valid stage3$valid stage4$valid stage5$valid stage6$valid
        stage7$valid)) |}];
       test
         ~config
         ~sim
         ~montgomery:false
         [ test_input ];
       [%expect {|
      (Ok ()) |}]
     ;;
   ]}
*)
