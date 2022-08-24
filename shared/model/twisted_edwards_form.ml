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
    let x3 = c_A * c_F * (((x1 + y1) * (x2 * y2)) - c_C - c_D) in
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
    { a : Z.t
    ; b : Z.t
    ; s : Z.t
    ; alpha : Z.t
    }

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
  let ( * ) = modulo_mult in
  { x = (one + y) * modulo_inverse (one - y)
  ; y = (one + y) * modulo_inverse ((one - y) * x)
  }
;;

let montgomery_affine_to_twisted_edwards_affine { Montgomery_curve.x = u; y = v } =
  let open Modulo_ops in
  { Twisted_edwards_curve.x = u * modulo_inverse v
  ; y = (u - one) * modulo_inverse (u + one)
  }
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

let montgomery_params_to_weierstrass_params
    ~alpha
    ({ c_A; c_B } : Montgomery_curve.params)
    : Weierstrass_curve.params
  =
  let open Modulo_ops in
  let a = (of_int 3 - (c_A * c_A)) / (of_int 3 * (c_B * c_B)) in
  let b = ((of_int 2 * (c_A * c_A * c_A)) - (of_int 9 * c_A)) / (of_int 27 * c_B * c_B) in
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
    Stdio.print_s
      [%message (obtained : Ark_bls12_377_g1.affine) (expected : Ark_bls12_377_g1.affine)];
    assert (Ark_bls12_377_g1.equal_affine obtained expected)
  in
  let a = Ark_bls12_377_g1.mul ~by:2 (Ark_bls12_377_g1.subgroup_generator ()) in
  let b = Ark_bls12_377_g1.subgroup_generator () in
  test a b
;;
