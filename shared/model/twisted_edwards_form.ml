open! Base
open Util

module Twisted_edwards_curve = struct
  type params =
    { a : Z.t
    ; d : Z.t
    }

  type affine =
    { x : Z.t
    ; y : Z.t
    }

  type projective =
    { x : Z.t
    ; y : Z.t
    ; z : Z.t
    }

  let affine_to_projective ({ x; y } : affine) : projective = { x; y; z = Z.one }

  let projective_to_affine { x; y; z } : affine =
    let open Modulo_ops in
    { x = x / z; y = y / z }
  ;;

  let add_not_equal { a; d } { x = x1; y = y1; z = z1 } { x = x2; y = y2; z = z2 } =
    let open Modulo_ops in
    let c_A = z1 * z2 in
    let c_B = c_A * c_A in
    let c_C = x1 * x2 in
    let c_D = y1 * y2 in
    let c_E = d * c_C * c_D in
    let c_F = c_B - c_E in
    let c_G = c_B + c_E in
    let x3 = c_A * c_F * (((x1 + y1) * (x2 + y2)) - c_C - c_D) in
    let y3 = c_A * c_G * (c_D - (a * c_C)) in
    let z3 = c_F * c_G in
    { x = x3; y = y3; z = z3 }
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

let%expect_test "Proof that 3 has a square root" =
  Stdio.printf "%s" (Z.to_string (modulo_pow Z.(of_int 3) Z.((p - of_int 1) / of_int 2)));
  [%expect {| 1 |}]
;;

let%expect_test "" =
  Stdio.printf "%s" (Z.to_string (Z.( mod ) Util.p (Z.of_int 4)));
  [%expect {| 1 |}]
;;

let%expect_test "" =
  Stdio.printf "%s" (Z.to_string (Z.( mod ) Util.p (Z.of_int 4)));
  [%expect {| 1 |}]
;;

let%expect_test "Modulo square root" =
  let y = Z.of_int 3 in
  let x = modulo_square_root y in
  Stdio.print_s (sexp_of_z x);
  [%expect
    {| 0x32d756062d349e59416ece15ccbf8e86ef0d33183465a42fe2cb65fc1664272e6bb28f0e1c7a7c9c05824ad09adc01 |}];
  Stdio.printf "%s" (Z.to_string (Util.modulo_mult x x));
  [%expect {| 3 |}];
  let x = modulo_pow (Z.of_int 3) (Z.of_int 3) in
  Stdio.printf "%s" (Z.to_string x);
  [%expect {| 27 |}];
  Stdio.printf "%s" (Z.to_string Util.p);
  [%expect
    {| 258664426012969094010652733694893533536393512754914660539884262666720468348340822774968888139573360124440321458177 |}];
  Stdio.printf "%s" (Z.to_string (Util.half Z.(p + one)));
  [%expect
    {| 129332213006484547005326366847446766768196756377457330269942131333360234174170411387484444069786680062220160729089 |}]
;;

let%expect_test "Params" =
  let bls12_377_params =
    Weierstrass_curve.create_params ~a:Z.zero ~b:Z.one ~alpha:Z.minus_one
  in
  Stdio.print_s [%message (bls12_377_params : Weierstrass_curve.params)];
  [%expect
    {|
    (bls12_377_params
     ((a 0x0) (b 0x1)
      (s
       0x10f272020f118a1dc07a44b1eeea84d7a504665d66cc8c0ff643cca95ccc0d0f793b8504b428d43401d618f0339eab)
      (alpha 0x-1))) |}];
  Stdio.print_s
    [%message
      (Util.modulo_mult
         (Util.modulo_inverse bls12_377_params.s)
         (Util.modulo_inverse bls12_377_params.s)
        : Util.z)];
  [%expect
    {|
    ( "Util.modulo_mult (Util.modulo_inverse bls12_377_params.s)\
     \n  (Util.modulo_inverse bls12_377_params.s)" 0x3) |}]
;;

let%expect_test "" =
  let bls12_377_params =
    Weierstrass_curve.create_params ~a:Z.zero ~b:Z.one ~alpha:Z.minus_one
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
        Twisted_edwards_curve.add_not_equal
          bls12_377_twisted_edwards_params
          (Twisted_edwards_curve.affine_to_projective a)
          (Twisted_edwards_curve.affine_to_projective b)
        |> Twisted_edwards_curve.projective_to_affine
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
    (Ark_bls12_377_g1.mul ~by:2 (Ark_bls12_377_g1.subgroup_generator ()))
    (Ark_bls12_377_g1.subgroup_generator ());
  [%expect
    {|
    ((x
      0x1252b781171f507db36291b433a1f911a46543890a20ca9712e11f66a5d216e63d817bd8d96cef715abc604dcf6ec2e)
     (y
      0x14a00fa77c727e8987cc438b51bbe012c823a19955ae692c54ce572a61f0ea1fe5cd981533df419fd1330d1f6e6d802)
     (infinity false)) |}]
;;
