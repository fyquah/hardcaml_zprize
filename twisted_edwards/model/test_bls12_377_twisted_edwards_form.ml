open Core
open Bls12_377_util
module C = Conversions

let%expect_test "Modulo square root" =
  let y = Z.of_int 3 in
  let x = modulo_square_root y in
  Stdio.print_s (sexp_of_z x);
  [%expect
    {| 0x17b62f01197dc4c6cf996f256d489ac9333ccbfe8c0adeaef1096c9bdf2e3d8e89faab521e38583e9033db52f652400 |}];
  Stdio.printf "%s" (Z.to_string (modulo_mult x x));
  [%expect {| 3 |}]
;;

let%expect_test "bls12-377 params in various forms" =
  Stdio.print_s [%message (p : z)];
  [%expect
    {|
    (p
     0x1ae3a4617c510eac63b05c06ca1493b1a22d9f300f5138f1ef3622fba094800170b5d44300000008508c00000000001) |}];
  let bls12_377_params = Lazy.force Bls12_377_params.weierstrass in
  Stdio.print_s ([%sexp_of: Weierstrass_curve.params] bls12_377_params);
  [%expect
    {|
    ((a 0x0) (b 0x1)
     (s
      0x19d47d415b5ff60a87a8b7bbab25eb6427dd58ca38e47030efd1e6310ac7bf3079221bf2b4bd72c5106e9e70fcc6156)
     (alpha 0x-1)) |}];
  let bls12_377_twisted_edwards_params = Lazy.force Bls12_377_params.twisted_edwards in
  Stdio.print_s
    ([%sexp_of: Twisted_edwards_curve.params] bls12_377_twisted_edwards_params);
  [%expect
    {|
    ((a
      0x1488b9a0b6aa7ae13b828244107ca1e0c44bf8cd08c4846bf2dcb63c1dc7fb1ba33f82613c70b074cfdbb6a5eca47fc)
     (d
      0x65aeac0c5a693cb282dd9c2b997f1d0dde1a663068cb485fc596cbf82cc84e5cd7651e1c38f4f9380b0495a135b7ff)) |}]
;;

let generate_with_probability ~p ~then_ ~else_ =
  let%bind.Quickcheck.Generator x = Float.gen_incl 0.0 1.0 in
  if Float.(x < p) then then_ else else_
;;

let generate_sample choices =
  let len = Array.length choices in
  let%bind.Quickcheck.Generator i = Int.gen_incl 0 (len - 1) in
  Quickcheck.Generator.return choices.(i)
;;

let generate_fp_special = generate_sample [| Z.one; Z.(p - one); Z.of_int 2 |]

let ark_bls12_377_g1_to_twisted_edewards_affine p1 =
  if Ark_bls12_377_g1.infinity p1
  then Twisted_edwards_curve.affine_identity
  else
    C.weierstrass_affine_to_twisted_edwards_affine
      (Lazy.force Bls12_377_params.weierstrass)
      { x = Ark_bls12_377_g1.x p1; y = Ark_bls12_377_g1.y p1 }
;;

let generate_fp_not_zero = generate_z ~lo_incl:Z.one ~hi_incl:Z.(p - one)

let generate_z_factor =
  generate_with_probability ~p:0.90 ~then_:generate_fp_not_zero ~else_:generate_fp_special
;;

let generate_point =
  (* We will not get infinity points, so scaling by zero doesn't make sense. *)
  let%bind.Quickcheck.Generator by = Int.gen_incl 1 Int.max_value in
  Ark_bls12_377_g1.mul (Ark_bls12_377_g1.subgroup_generator ()) ~by
  |> Quickcheck.Generator.return
;;

let%expect_test "Verify that res_extended.x = 0 (ignore y) where res_extended = p + -p" =
  let bls12_377_twisted_edwards_params = Lazy.force Bls12_377_params.twisted_edwards in
  let test ~z p1 =
    let p2 = Ark_bls12_377_g1.neg p1 in
    let obtained =
      let a = ark_bls12_377_g1_to_twisted_edewards_affine p1 in
      let b = ark_bls12_377_g1_to_twisted_edewards_affine p2 in
      Twisted_edwards_curve.add_unified
        bls12_377_twisted_edwards_params
        (Twisted_edwards_curve.affine_to_extended ~z a)
        (Twisted_edwards_curve.affine_to_affine_with_t b)
    in
    assert (Z.equal obtained.x Z.zero)
  in
  let generate =
    let%bind.Quickcheck.Generator p1 = generate_point in
    let%bind.Quickcheck.Generator z = generate_z_factor in
    Quickcheck.Generator.return (p1, z)
  in
  Quickcheck.test
    ~sexp_of:[%sexp_of: Ark_bls12_377_g1.affine * z]
    ~trials:10_000
    generate
    ~f:(fun (p1, z) -> test p1 ~z)
;;

let%expect_test "Randomized bls12-377 adding equivalence with wierstrass form addition" =
  let bls12_377_twisted_edwards_params = Lazy.force Bls12_377_params.twisted_edwards in
  let test ~z a b =
    let obtained_in_twisted_edwards_form =
      let a = ark_bls12_377_g1_to_twisted_edewards_affine a in
      let b = ark_bls12_377_g1_to_twisted_edewards_affine b in
      Twisted_edwards_curve.add_unified
        bls12_377_twisted_edwards_params
        (Twisted_edwards_curve.affine_to_extended ~z a)
        (Twisted_edwards_curve.affine_to_affine_with_t b)
      |> Twisted_edwards_curve.extended_to_affine
    in
    let obtained =
      match
        obtained_in_twisted_edwards_form
        |> C.twisted_edwards_affine_to_weierstrass_affine bls12_377_twisted_edwards_params
      with
      | None -> Ark_bls12_377_g1.create ~x:Z.zero ~y:Z.one ~infinity:true
      | Some res -> Ark_bls12_377_g1.create ~x:res.x ~y:res.y ~infinity:false
    in
    let expected = Ark_bls12_377_g1.add a b in
    if not (Ark_bls12_377_g1.equal_affine obtained expected)
    then
      raise_s
        [%message
          "Obtained and expected mismatches"
            (a : Ark_bls12_377_g1.affine)
            (b : Ark_bls12_377_g1.affine)
            (obtained_in_twisted_edwards_form : Twisted_edwards_curve.affine)
            (obtained : Ark_bls12_377_g1.affine)
            (expected : Ark_bls12_377_g1.affine)]
  in
  let generate_point =
    let%bind.Quickcheck.Generator by =
      (* We will not get infinity points, so scaling by zero doesn't make sense. *)
      Int.gen_incl 1 Int.max_value
    in
    Ark_bls12_377_g1.mul (Ark_bls12_377_g1.subgroup_generator ()) ~by
    |> Quickcheck.Generator.return
  in
  let generate =
    let open Quickcheck.Generator.Let_syntax in
    let pidentity = Ark_bls12_377_g1.create ~x:Z.zero ~y:Z.zero ~infinity:true in
    let%bind p1 =
      generate_with_probability ~p:0.99 ~then_:generate_point ~else_:(return pidentity)
    in
    let%bind p2 =
      generate_with_probability
        ~p:0.90
        ~then_:(* Do a "regular" differential addition most of the time. *)
               generate_point
        ~else_:
          ((* Exercise various odd cases in this test:
               - p1 + O
               - p1 + p1
               - p1 + (-p1)
            *)
           generate_with_probability
             ~p:0.30
             ~then_:(return pidentity)
             ~else_:
               (generate_with_probability
                  ~p:0.30
                  ~then_:(return p1)
                  ~else_:(return (Ark_bls12_377_g1.neg p1))))
    in
    let%bind.Quickcheck.Generator z = generate_z_factor in
    Quickcheck.Generator.return (p1, p2, z)
  in
  Quickcheck.test
    ~sexp_of:[%sexp_of: Ark_bls12_377_g1.affine * Ark_bls12_377_g1.affine * z]
    ~trials:10_000
    generate
    ~f:(fun (p1, p2, z) -> test p1 p2 ~z)
;;
