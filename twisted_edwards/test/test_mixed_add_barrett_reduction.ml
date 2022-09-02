open Core
open Test_mixed_add

let random_z = Field_ops_test.Utils.random_z
let p = Ark_bls12_377_g1.modulus ()

let xyzt_to_xyt xyzt =
  let ({ x; y } : Model.Twisted_edwards_curve.affine) =
    let { Xyzt.x; y; z; t } = xyzt in
    Model.Twisted_edwards_curve.extended_to_affine { x; y; z; t }
  in
  let t = Model.Bls12_377_util.modulo_mult x y in
  { Mixed_add.Xyt.x; y; t }
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

let xyt_neg { Mixed_add.Xyt.x; y; t } =
  { Mixed_add.Xyt.x = Model.Bls12_377_util.modulo_neg x
  ; y
  ; t = Model.Bls12_377_util.modulo_neg t
  }
;;

let%expect_test "Latency" =
  Stdio.printf
    "Latency = %d\n"
    (Test_mixed_add.Mixed_add.latency
       (Lazy.force Config.For_bls12_377.with_barrett_reduction));
  [%expect {| Latency = 165 |}]
;;

let%expect_test "Test on some test cases" =
  Random.init 123;
  let test_cases =
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
        p1, p2)
  in
  let config = Lazy.force Config.For_bls12_377.with_barrett_reduction in
  let sim = create_sim config in
  test ~montgomery:false ~config ~sim test_cases;
  [%expect {| (Ok ()) |}]
;;
