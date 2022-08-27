open! Base
open Util

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

let twisted_edward_params_to_weierstrass_params ~alpha p =
  montgomery_params_to_weierstrass_params
    ~alpha
    (twisted_edwards_params_to_montgomery_params p)
;;
