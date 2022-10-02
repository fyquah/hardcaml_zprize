open Core
open Test_mixed_add
module Xyzt = Test_mixed_add.Xyzt
module Xyt = Test_mixed_add.Xyt

let random_z = Field_ops_test.Utils.random_z
let p = Ark_bls12_377_g1.modulus ()

let xyzt_to_xyt xyzt =
  let ({ x; y } : Model.Twisted_edwards_curve.affine) =
    let { Xyzt.x; y; z; t } = xyzt in
    Model.Twisted_edwards_curve.extended_to_affine { x; y; z; t }
  in
  let t = Model.Bls12_377_util.modulo_mult x y in
  { Xyt.x; y; t }
;;

let random_non_zero () = random_z ~lo_incl:Z.one ~hi_incl:Z.(p - one)

let random_xyt () =
  let x = random_non_zero () in
  let y = random_non_zero () in
  let t = Model.Bls12_377_util.modulo_mult x y in
  { Xyt.x; y; t }
;;

let random_xyzt () =
  let { Xyt.x; y; t } = random_xyt () in
  let z = random_non_zero () in
  let modulo_mult = Model.Bls12_377_util.modulo_mult in
  { Xyzt.x = modulo_mult x z; y = modulo_mult y z; t = modulo_mult t z; z }
;;

let xyt_neg { Xyt.x; y; t } =
  { Xyt.x = Model.Bls12_377_util.modulo_neg x; y; t = Model.Bls12_377_util.modulo_neg t }
;;

let random_test_cases =
  Random.init 123;
  List.init 1_000 ~f:(fun _ ->
    let p1 =
      let a = Random.float 1.0 in
      if Float.(a < 0.99)
      then random_xyzt ()
      else (
        let z = random_non_zero () in
        { Xyzt.x = Z.zero; y = z; t = Z.zero; z })
    in
    let p2 =
      let p1 = lazy (xyzt_to_xyt p1) in
      let a = Random.float 1.0 in
      if Float.(a < 0.80)
      then random_xyt ()
      else if Float.(a < 0.95)
      then (* Exercise doubling *)
        Lazy.force p1
      else if Float.(a < 0.98)
      then (* Exercise x + -x *)
        xyt_neg (Lazy.force p1)
      else (* Exercise x + O *)
        { Xyt.x = Z.zero; y = Z.one; t = Z.zero }
    in
    p1, p2, Random.bool ())
;;

type test_type =
  | Mixed_add
  | Mixed_add_precompute of { arbitrate : bool }

let test test_type (test_cases : (Z.t Xyzt.t * Z.t Xyt.t * bool) list) =
  match test_type with
  | Mixed_add_precompute { arbitrate } ->
    if arbitrate
    then (
      let config = Lazy.force Config.For_bls12_377.with_barrett_reduction_arbitrated in
      let open Test_mixed_add_precompute in
      let sim = create_sim config in
      let test_cases =
        List.map
          ~f:(fun (p1, p2, subtract) ->
            ( { Xyzt.x = p1.x; y = p1.y; z = p1.z; t = p1.t }
            , { Xyt.x = p2.x; y = p2.y; t = p2.t }
            , subtract ))
          test_cases
      in
      test ~montgomery:false ~config ~sim test_cases)
    else (
      let config = Lazy.force Config.For_bls12_377.with_barrett_reduction_full in
      let open Test_mixed_add_precompute_full in
      let sim = create_sim config in
      let test_cases =
        List.map
          ~f:(fun (p1, p2, subtract) ->
            ( { Xyzt.x = p1.x; y = p1.y; z = p1.z; t = p1.t }
            , { Xyt.x = p2.x; y = p2.y; t = p2.t }
            , subtract ))
          test_cases
      in
      test ~montgomery:false ~config ~sim test_cases)
  | Mixed_add ->
    let config = Lazy.force Config.For_bls12_377.with_barrett_reduction_arbitrated in
    let open Test_mixed_add in
    let sim = create_sim config in
    test ~montgomery:false ~config ~sim test_cases
;;

let test_random test_type = test test_type random_test_cases

let%expect_test "Latency of half adder" =
  Stdio.printf
    "%d"
    (Test_mixed_add.Adder.latency
       (Lazy.force Config.For_bls12_377.with_barrett_reduction_arbitrated));
  [%expect {| 238 |}]
;;

let%expect_test "Latency of full adder" =
  Stdio.printf
    "%d"
    (Test_mixed_add.Adder.latency
       (Lazy.force Config.For_bls12_377.with_barrett_reduction_full));
  [%expect {| 238 |}]
;;

let%expect_test "Test on some test cases (without host precompute)" =
  test_random Mixed_add;
  [%expect {| (Ok ()) |}]
;;

let%expect_test "Test on some test cases (with host precompute)" =
  test_random (Mixed_add_precompute { arbitrate = true });
  [%expect {| (Ok ()) |}]
;;

let%expect_test "Test on some test cases (with host precompute and full adder)" =
  test_random (Mixed_add_precompute { arbitrate = false });
  [%expect {| (Ok ()) |}]
;;

let%expect_test "possible regression" =
  List.iter
    [ Mixed_add_precompute { arbitrate = true }
    ; Mixed_add
    ; Mixed_add_precompute { arbitrate = false }
    ]
    ~f:(fun test_type ->
      test
        test_type
        [ ( { x = Z.zero; y = Z.one; z = Z.one; t = Z.zero }
          , { x =
                Z.of_string
                  "0x91cc1f89c5ba2b00ec8a766f03f21d3b0ad0791c3b93f6e584cc9f29f7ebefc7468c91c6e7bd8cd1c7e1309fa963e8"
            ; y =
                Z.of_string
                  "0x5980ae72bf91bc1e388402d190de3b7e2e459dfa82f4bb815243363443c5fa61abeb8578c333753225da747ec63bdc"
            ; t =
                Z.of_string
                  "0x15e283ceaf5a24978133482e18dadbd18df020a469a1d0430e78f01f100023f49db5202cd8c6f2d6caedcd90dc33cb7"
            }
          , false )
        ]);
  [%expect {|
    (Ok ())
    (Ok ())
    (Ok ()) |}]
;;
