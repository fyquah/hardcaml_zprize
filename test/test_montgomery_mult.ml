open! Core
open! Hardcaml
open! Snarks_r_fun

module Montgomery_mult377 = Montgomery_mult.With_interface(struct
    let bits = 377
  end)
module Sim = Cyclesim.With_interface(Montgomery_mult377.I)(Montgomery_mult377.O)

let create_sim ~p ~config =
  let scope = Scope.create ~flatten_design:true () in
  Sim.create ~config:Cyclesim.Config.trace_all
    (Montgomery_mult377.create ~p ~config scope)
;;

type test_cases =
  { x : Z.t
  ; y : Z.t
  }

let debug = false

(* Software model of the REDC algorithm. *)
let compute_software_model ~logr ~p x y =
  let r = Z.(one lsl logr) in
  let p' =
    let { Extended_euclidean. coef_x = _ ; coef_y; gcd } =
      Extended_euclidean.extended_euclidean ~x:r ~y:p
    in
    assert (Z.equal gcd Z.one);
    Z.(neg coef_y mod r)
  in
  let p' =
    if Z.(lt p' zero) then
      Z.(p' + r)
    else
      p'
  in
  (* p'p = -1 mod r. Since we are using Zarith, having signed integers here
   * is fine. 
   *)
  assert Z.(equal ((p * p') mod r) Z.(r - one));
  let xy = Z.(x * y) in
  if debug then (
    Stdio.printf !"expected[xy] = %{Sexp}\n" (Utils.sexp_of_z xy);
  );
  let m = Z.((((xy mod r) * p') + r) mod r) in
  if debug then (
    Stdio.printf !"expected[m] = %{Sexp}\n" (Utils.sexp_of_z m);
  );
  let mp = Z.(m * p) in
  if debug then (
    Stdio.printf !"expected[mp] = %{Sexp}\n" (Utils.sexp_of_z mp);
  );
  let xy_plus_mp = Z.(xy + mp) in
  if debug then (
    Stdio.printf !"expected[x + mp] = %{Sexp}\n" (Utils.sexp_of_z xy_plus_mp);
  );
  let t = Z.(xy_plus_mp / r) in
  if debug then (
    Stdio.printf !"expected[t] = %{Sexp}\n" (Utils.sexp_of_z t);
  );
  let result = Z.(t mod p) in
  if Z.lt result Z.zero then
    Z.(result + p)
  else
    result
;;

(* The expected result of xyr' mod p *)
let compute_expected ~logr ~p x y =
  let r = Z.(one lsl logr) in
  let r' =
    let { Extended_euclidean. coef_x ; coef_y = _; gcd } =
      Extended_euclidean.extended_euclidean ~x:r ~y:p
    in
    assert (Z.equal Z.one gcd);
    if Z.lt coef_x Z.zero then
      Z.(+) coef_x p
    else
      coef_x
  in
  Z.((x * y * r') mod p)
;;

let p = Ark_bls12_377_g1.modulus ()

let random_bigint () =
  Utils.random_z ~lo_incl:Z.zero ~hi_incl:Z.(p - one)
;;

let%expect_test _ =
  let config =
    { Montgomery_mult.Config.
      multiplier_depth = 3
    ; adder_depth = 1
    ; subtracter_depth = 1
    }
  in
  let test_cases =
    let hand_crafted =
      [ { x = Z.of_string "1"
        ; y = Z.of_string "1"
        }
      ; { x = Z.(p - one)
        ; y = Z.(p - one)
        }
      ]
    in
    let auto_generated =
      List.init 50 ~f:(fun _ ->
          { x = random_bigint ()
          ; y = random_bigint ()
          })
    in
    auto_generated @ hand_crafted
  in
  let sim = create_sim ~p ~config in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  if debug then (
    Stdio.print_s (
      [%sexp_of: string list]
        (List.sort ~compare:String.compare (List.map ~f:fst (Cyclesim.internal_ports sim))))
  );
  [%expect {| |}];
  let internal_ports = Cyclesim.internal_ports sim in
  let find_internal_port s = List.Assoc.find_exn internal_ports ~equal:[%equal: string] s in
  let find_and_print_port s =
    Stdio.printf !"%s = %{Sexp}\n"
      s
      (Utils.sexp_of_z (Bits.to_z ~signedness:Unsigned !(find_internal_port s)))
  in
  let is_internal_port_vdd name = Bits.is_vdd !(find_internal_port name) in
  let results = Queue.create () in
  let cycle () =
    Cyclesim.cycle sim;
    if Bits.is_vdd !(outputs.valid) then (
      Queue.enqueue results (Bits.to_z ~signedness:Unsigned !(outputs.z))
    );
    (* Debugging outputs ... *)
    if debug then (
      if is_internal_port_vdd "stage1$valid" then (
        find_and_print_port "stage1$xy";
      );
      if is_internal_port_vdd "stage2$valid" then (
        find_and_print_port "stage2$m";
        find_and_print_port "stage2$xy";
      );
      if is_internal_port_vdd "stage3$valid" then (
        find_and_print_port "stage3$mp";
        find_and_print_port "stage3$xy";
      );
      if is_internal_port_vdd "stage4$valid" then (
        find_and_print_port "stage4$t";
        find_and_print_port "stage4$xy_plus_mp";
      );
      if is_internal_port_vdd "stage5$valid" then (
        find_and_print_port "stage5$result";
      );
    )
  in
  inputs.enable := Bits.vdd;
  List.iter test_cases ~f:(fun { x; y } ->
      inputs.x := Bits.of_z ~width:Montgomery_mult377.bits x;
      inputs.y := Bits.of_z ~width:Montgomery_mult377.bits y;
      inputs.valid := Bits.vdd;
      cycle ();
    );
  inputs.valid := Bits.gnd;
  for _ = 1 to Montgomery_mult.Config.latency config do
    cycle ();
  done;
  cycle ();
  List.map2_exn test_cases (Queue.to_list results)
    ~f:(fun { x; y } obtained ->
        let expected = compute_expected ~p ~logr:Montgomery_mult377.bits x y in
        if Z.equal obtained expected
        then Ok ()
        else
          let software_model = compute_software_model ~p ~logr:Montgomery_mult377.bits x y in
          Or_error.error_s [%message
            "Test case failed!"
              (x : Utils.z)
              (y : Utils.z)
              (obtained : Utils.z)
              (expected : Utils.z)
              (software_model : Utils.z)
          ])
  |> Or_error.combine_errors_unit
  |> [%sexp_of: unit Or_error.t]
  |> Stdio.print_s;
  [%expect {|
    (Error
     (("Test case failed!"
       (x
        0x13b028d2e7241fe165c10c3f58d6deb51931da8363f6e1742b7c0bf033ec7e48ff0024148f3ecf4fca19095c344fb6d)
       (y
        0x1ae3a4617c510eac63b05c06ca1493b1a22d9f300f5138f1ef3622fba094800170b5d44300000008508c00000000000)
       (obtained
        0x3e99901059424c32a1829b5c14756ca4f89e4507afa6efbd7ef42d2b4bee872e09fdb81187acabc1f37c230aebebd9)
       (expected
        0x905f49f89431616c667cdaef732c318ad5c45206ba93609e8b91fd7142a68716fea073e187acab3ceabc230aebebd8)
       (software_model
        0x905f49f89431616c667cdaef732c318ad5c45206ba93609e8b91fd7142a68716fea073e187acab3ceabc230aebebd8))
      ("Test case failed!"
       (x
        0x19c46f53ec4890afb8a3a5d6fbe1b31b61e2ea32114b8a8f3b323da35e68eb15e832d9f8e51b33d51735d92b5465ee6)
       (y
        0x166772060e021e84233c15ebdb21d7ceac897561d85aa7ee56339b6d7220dede2340f34db0623e97473e9645834f34c)
       (obtained
        0xa34d38815660b5ff5f44a09b4fdf208180d8cf8a91f323969423b34b54a1a451fe6c4e6356f3381def00fb6955d039)
       (expected
        0xf512f269914fcb39243ee02eae95e5675dfedc899cdf9477a0c183914b59a43af30f0a3356f33798e640fb6955d038)
       (software_model
        0xf512f269914fcb39243ee02eae95e5675dfedc899cdf9477a0c183914b59a43af30f0a3356f33798e640fb6955d038))
      ("Test case failed!"
       (x
        0x151e2d6583c8da586b102daf27e3a74b9eaf116782f26872e60737658e446b4c586baec53ce37689c7376f51bbd06b0)
       (y
        0x101d3a5eef9471c8ee1542d782091c619b13ef5dd0f8d8a4a77d4a3ff2a433c835baa7f29f8116bb6ad12c7c67ccc91)
       (obtained
        0x3917d0e179ac41e6d7c7177b8b34e91bce89ef5b119de6bd2e4771447688459b1fc316bfaff4d1634cc570c8953a94)
       (expected
        0x8add8ac9b49b57209cc1570ee9ebae01abaffc5a1c8a579e3ae5418a6d4045841465d28faff4d0de440570c8953a93)
       (software_model
        0x8add8ac9b49b57209cc1570ee9ebae01abaffc5a1c8a579e3ae5418a6d4045841465d28faff4d0de440570c8953a93))
      ("Test case failed!"
       (x
        0x1ae3a4617c510eac63b05c06ca1493b1a22d9f300f5138f1ef3622fba094800170b5d44300000008508c00000000000)
       (y
        0xeb0afc47639fac21ad56dae646a869ed76e79805af07750a3aabe0189313f9588a8ac9159610bfdd4d4ee4c62365d8)
       (obtained
        0x6de4e56863ab4c7da10f14a51722b5e748962d9baf3134de341cbfd6fb75d0dbecbec343706d0168bc047bbaa4459a)
       (expected
        0xbfaa9f509e9a61b76609543875d97acd25bc3a9aba1da5bf40ba901cf22dd0c4e1617f13706d00e3b3447bbaa44599)
       (software_model
        0xbfaa9f509e9a61b76609543875d97acd25bc3a9aba1da5bf40ba901cf22dd0c4e1617f13706d00e3b3447bbaa44599))
      ("Test case failed!"
       (x
        0x1081211aaa7024d8191eee89cbb32580dd2eff7fe9fe90f4daa10936ba4a49dd0efd9b26a8b9b02fed14b42a0e257d6)
       (y
        0x1ae3a4617c510eac63b05c06ca1493b1a22d9f300f5138f1ef3622fba094800170b5d44300000008508c00000000000)
       (obtained
        0x2ad13d0bfa506b8a4256516b52d7423e3c4c9b146f69b832ddc0fd1b890d9e2a35e44d3204a775867c49634c7647dc)
       (expected
        0x7c96f6f4353f80c4075090feb18e07241972a8137a562913ea5ecd617fc59e132a87090204a775017389634c7647db)
       (software_model
        0x7c96f6f4353f80c4075090feb18e07241972a8137a562913ea5ecd617fc59e132a87090204a775017389634c7647db))
      ("Test case failed!"
       (x
        0x1ae3a4617c510eac63b05c06ca1493b1a22d9f300f5138f1ef3622fba094800170b5d44300000008508c00000000000)
       (y
        0x1ae3a4617c510eac63b05c06ca1493b1a22d9f300f5138f1ef3622fba094800170b5d44300000008508c00000000000)
       (obtained
        0x1101864fb276540fb0e19aa7f10c543bcae07c19f75fd3539c036cc5cadd9ae9a31a7e98c1cfac6b011c9a28934f3a2)
       (expected
        0x161de1ee362545634d313ea126f7c08a28b2dce9e80e9a61accd49ca2a491ae83264aa55c1cfac62b0909a28934f3a1)
       (software_model
        0x161de1ee362545634d313ea126f7c08a28b2dce9e80e9a61accd49ca2a491ae83264aa55c1cfac62b0909a28934f3a1)))) |}]
;;
