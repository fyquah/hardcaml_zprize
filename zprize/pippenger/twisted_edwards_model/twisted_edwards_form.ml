open! Base
open Util

module Twisted_edwards_curve = struct
  type params =
    { a : Util.z
    ; d : Util.z
    }
  [@@deriving sexp_of]

  type affine =
    { x : Z.t
    ; y : Z.t
    }

  type affine_with_t =
    { x : Z.t
    ; y : Z.t
    ; t : Z.t
    }

  type extended =
    { x : Util.z
    ; y : Util.z
    ; z : Util.z
    ; t : Util.z
    }
  [@@deriving sexp_of]

  let affine_to_extended ~z ({ x; y } : affine) : extended =
    { x = modulo_mult x z; y = modulo_mult y z; z; t = modulo_mult (modulo_mult x y) z }
  ;;

  let affine_to_affine_with_t ({ x; y } : affine) : affine_with_t =
    { x; y; t = modulo_mult x y }
  ;;

  let extended_to_affine { x; y; z; t } : affine =
    let open Modulo_ops in
    assert (equal (x / z * (y / z)) (t / z));
    { x = x / z; y = y / z }
  ;;

  let _add_not_equal
      { a; d = _ }
      ({ x = x1; y = y1; z = z1; t = t1 } : extended)
      ({ x = x2; y = y2; t = t2 } : affine_with_t)
      : extended
    =
    (* https://hyperelliptic.org/EFD/g1p/auto-twisted-extended.html#addition-madd-2008-hwcd-2 *)
    let open Modulo_ops in
    let c_A = x1 * x2 in
    let c_B = y1 * y2 in
    let c_C = z1 * t2 in
    let c_D = t1 in
    let c_E = c_D + c_C in
    let c_F = ((x1 - y1) * (x2 + y2)) + c_B - c_A in
    let c_G = c_B + (a * c_A) in
    let c_H = c_D - c_C in
    let x3 = c_E * c_F in
    let y3 = c_G * c_H in
    let t3 = c_E * c_H in
    let z3 = c_F * c_G in
    { x = x3; y = y3; z = z3; t = t3 }
  ;;

  let add_unified
      { a; d }
      ({ x = x1; y = y1; z = z1; t = t1 } : extended)
      ({ x = x2; y = y2; t = t2 } : affine_with_t)
      : extended
    =
    let open Modulo_ops in
    let c_A = x1 * x2 in
    let c_B = y1 * y2 in
    let c_C = t1 * d * t2 in
    let c_D = z1 in
    let c_E = ((x1 + y1) * (x2 + y2)) - c_A - c_B in
    let c_F = c_D - c_C in
    let c_G = c_D + c_C in
    let c_H = c_B - (a * c_A) in
    let x3 = c_E * c_F in
    let y3 = c_G * c_H in
    let t3 = c_E * c_H in
    let z3 = c_F * c_G in
    { x = x3; y = y3; z = z3; t = t3 }
  ;;
end

module Montgomery_curve = struct
  type params =
    { c_A : Z.t
    ; c_B : Z.t
    }

  type affine =
    { x : Z.t
    ; y : Z.t
    }
end

module Weierstrass_curve = struct
  type params =
    { a : Util.z
    ; b : Util.z
    ; s : Util.z
    ; alpha : Util.z
    }
  [@@deriving sexp_of]

  let create_params ~a ~b ~alpha =
    let open Modulo_ops in
    let s = modulo_inverse (modulo_square_root ((of_int 3 * alpha * alpha) + a)) in
    { a; b; s; alpha }
  ;;

  type affine =
    { x : Z.t
    ; y : Z.t
    }
end

let twisted_edwards_affine_to_montgomery_affine ({ x; y } : Twisted_edwards_curve.affine)
    : Montgomery_curve.affine
  =
  let open Modulo_ops in
  { x = (one + y) / (one - y); y = (one + y) / ((one - y) * x) }
;;

let montgomery_affine_to_twisted_edwards_affine { Montgomery_curve.x = u; y = v } =
  let open Modulo_ops in
  { Twisted_edwards_curve.x = u / v; y = (u - one) / (u + one) }
;;

let weierstrass_affine_to_montgomery_affine
    ({ s; alpha; _ } : Weierstrass_curve.params)
    ({ x = t; y = v } : Weierstrass_curve.affine)
    : Montgomery_curve.affine
  =
  let open Modulo_ops in
  let x = s * (t - alpha) in
  let y = s * v in
  { x; y }
;;

let montgomery_affine_to_weierstrass_affine
    ({ c_A; c_B } : Montgomery_curve.params)
    ({ x; y } : Montgomery_curve.affine)
    : Weierstrass_curve.affine
  =
  let open Modulo_ops in
  let t = (x / c_B) + (c_A / (of_int 3 * c_B)) in
  let v = y / c_B in
  { x = t; y = v }
;;

let montgomery_params_to_twisted_edwards_params ({ c_A; c_B } : Montgomery_curve.params)
    : Twisted_edwards_curve.params
  =
  let open Modulo_ops in
  let a = (c_A + of_int 2) / c_B in
  let d = (c_A - of_int 2) / c_B in
  { a; d }
;;

let twisted_edwards_params_to_montgomery_params ({ a; d } : Twisted_edwards_curve.params)
    : Montgomery_curve.params
  =
  let open Modulo_ops in
  let c_A = of_int 2 * (a + d) / (a - d) in
  let c_B = of_int 4 / (a - d) in
  { c_A; c_B }
;;

let cubed x =
  let open Modulo_ops in
  x * x * x
;;

let square x =
  let open Modulo_ops in
  x * x
;;

let montgomery_params_to_weierstrass_params
    ~alpha
    ({ c_A; c_B } : Montgomery_curve.params)
    : Weierstrass_curve.params
  =
  let open Modulo_ops in
  let a = (of_int 3 - square c_A) / (of_int 3 * square c_B) in
  let b = ((of_int 2 * cubed c_A) - (of_int 9 * c_A)) / (of_int 27 * cubed c_B) in
  Weierstrass_curve.create_params ~a ~b ~alpha
;;

let weierstrass_params_to_montgomery_params
    ({ a = _; b = _; alpha; s } : Weierstrass_curve.params)
    : Montgomery_curve.params
  =
  let open Modulo_ops in
  let c_A = of_int 3 * alpha * s in
  let c_B = s in
  { c_A; c_B }
;;

let twisted_edwards_affine_to_weierstrass_affine twisted_edwards_params point =
  montgomery_affine_to_weierstrass_affine
    (twisted_edwards_params_to_montgomery_params twisted_edwards_params)
    (twisted_edwards_affine_to_montgomery_affine point)
;;

let weierstrass_affine_to_twisted_edwards_affine weierstrass_params point =
  montgomery_affine_to_twisted_edwards_affine
    (weierstrass_affine_to_montgomery_affine weierstrass_params point)
;;

let weierstrass_params_to_twisted_edwards_params p =
  montgomery_params_to_twisted_edwards_params (weierstrass_params_to_montgomery_params p)
;;

let _twisted_edward_params_to_weierstrass_params ~alpha p =
  montgomery_params_to_weierstrass_params
    ~alpha
    (twisted_edwards_params_to_montgomery_params p)
;;

let%expect_test "Modulo square root" =
  let y = Z.of_int 3 in
  let x = modulo_square_root y in
  Stdio.print_s (sexp_of_z x);
  [%expect
    {| 0x17b62f01197dc4c6cf996f256d489ac9333ccbfe8c0adeaef1096c9bdf2e3d8e89faab521e38583e9033db52f652400 |}];
  Stdio.printf "%s" (Z.to_string (Util.modulo_mult x x));
  [%expect {| 3 |}]
;;

let%expect_test "Params" =
  Stdio.print_s [%message (p : Util.z)];
  [%expect
    {|
    (p
     0x1ae3a4617c510eac63b05c06ca1493b1a22d9f300f5138f1ef3622fba094800170b5d44300000008508c00000000001) |}];
  let bls12_377_params =
    Weierstrass_curve.create_params ~a:Z.zero ~b:Z.one ~alpha:Z.minus_one
  in
  Stdio.print_s [%message (bls12_377_params : Weierstrass_curve.params)];
  [%expect
    {|
    (bls12_377_params
     ((a 0x0) (b 0x1)
      (s
       0x19d47d415b5ff60a87a8b7bbab25eb6427dd58ca38e47030efd1e6310ac7bf3079221bf2b4bd72c5106e9e70fcc6156)
      (alpha 0x-1))) |}];
  let bls12_377_twisted_edwards_params =
    weierstrass_params_to_twisted_edwards_params bls12_377_params
  in
  Stdio.print_s
    [%message (bls12_377_twisted_edwards_params : Twisted_edwards_curve.params)];
  [%expect
    {|
    (bls12_377_twisted_edwards_params
     ((a
       0x1488b9a0b6aa7ae13b828244107ca1e0c44bf8cd08c4846bf2dcb63c1dc7fb1ba33f82613c70b074cfdbb6a5eca47fc)
      (d
       0x65aeac0c5a693cb282dd9c2b997f1d0dde1a663068cb485fc596cbf82cc84e5cd7651e1c38f4f9380b0495a135b7ff))) |}]
;;

let%expect_test "" =
  let bls12_377_params =
    let alpha = Util.modulo_add Util.p Z.minus_one in
    Weierstrass_curve.create_params ~a:Z.zero ~b:Z.one ~alpha
  in
  let bls12_377_twisted_edwards_params =
    weierstrass_params_to_twisted_edwards_params bls12_377_params
  in
  let test a b =
    let obtained =
      let a =
        weierstrass_affine_to_twisted_edwards_affine
          bls12_377_params
          { x = Ark_bls12_377_g1.x a; y = Ark_bls12_377_g1.y a }
      in
      let b =
        weierstrass_affine_to_twisted_edwards_affine
          bls12_377_params
          { x = Ark_bls12_377_g1.x b; y = Ark_bls12_377_g1.y b }
      in
      let (res : Weierstrass_curve.affine) =
        let z = Util.random_z ~lo_incl:Z.zero ~hi_incl:Z.(Util.p - one) in
        Twisted_edwards_curve.add_unified
          bls12_377_twisted_edwards_params
          (Twisted_edwards_curve.affine_to_extended ~z a)
          (Twisted_edwards_curve.affine_to_affine_with_t b)
        |> Twisted_edwards_curve.extended_to_affine
        |> twisted_edwards_affine_to_weierstrass_affine bls12_377_twisted_edwards_params
      in
      Ark_bls12_377_g1.create ~x:res.x ~y:res.y ~infinity:false
    in
    let expected = Ark_bls12_377_g1.add a b in
    if Ark_bls12_377_g1.equal_affine obtained expected
    then Stdio.print_s ([%sexp_of: Ark_bls12_377_g1.affine] obtained)
    else
      raise_s
        [%message
          "Obtained and expected mismatches"
            (obtained : Ark_bls12_377_g1.affine)
            (expected : Ark_bls12_377_g1.affine)]
  in
  test
    (Ark_bls12_377_g1.mul ~by:120 (Ark_bls12_377_g1.subgroup_generator ()))
    (Ark_bls12_377_g1.mul ~by:3 (Ark_bls12_377_g1.subgroup_generator ()));
  [%expect
    {|
    ((x
      0x180640c09dd2a1f8a2ad648710067e6ac48b07fa23db4bd60579706b5e5ba6c38bf1c9d58aa48818cfdf07fd5767150)
     (y
      0x3ee1310eb124d9c189de356f1225ff8b1aa920b87afa927a2b2e9e1f232b0624fabbbfeb22442cb35decae0b701852)
     (infinity false)) |}]
;;
