open! Base
open Bls12_377_util

let twisted_edwards_affine_to_montgomery_affine
  ({ twisted_scale; _ } : Twisted_edwards_curve.params)
  ({ x; y } : Twisted_edwards_curve.affine)
  : Montgomery_curve.affine option
  =
  let open Modulo_ops in
  if Z.equal x Z.zero
  then None
  else Some { x = (one + y) / (one - y); y = (one + y) * twisted_scale / ((one - y) * x) }
;;

let montgomery_affine_to_twisted_edwards_affine
  ({ twisted_scale; _ } : Montgomery_curve.params)
  ({ x = u; y = v } : Montgomery_curve.affine)
  =
  let open Modulo_ops in
  { Twisted_edwards_curve.x = twisted_scale * u / v; y = (u - one) / (u + one) }
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
  ({ c_A; c_B; _ } : Montgomery_curve.params)
  ({ x; y } : Montgomery_curve.affine)
  : Weierstrass_curve.affine
  =
  let open Modulo_ops in
  let t = (x / c_B) + (c_A / (of_int 3 * c_B)) in
  let v = y / c_B in
  { x = t; y = v }
;;

let montgomery_params_to_twisted_edwards_params
  ({ c_A; c_B = _; twisted_scale } : Montgomery_curve.params)
  : Twisted_edwards_curve.params
  =
  let open Modulo_ops in
  let a = Bls12_377_util.p + of_int (-1) in
  let d = (of_int 2 - c_A) / (of_int 2 + c_A) in
  { a; d; twisted_scale }
;;

let twisted_edwards_params_to_montgomery_params
  ({ a = _; d; twisted_scale } : Twisted_edwards_curve.params)
  : Montgomery_curve.params
  =
  let open Modulo_ops in
  let c_A = (of_int 4 / (d + of_int 1)) - of_int 2 in
  let c_B = of_int (-1) * ((c_A + of_int 2) / square twisted_scale) in
  { c_A; c_B; twisted_scale }
;;

let montgomery_params_to_weierstrass_params
  ~alpha
  ({ c_A; c_B; twisted_scale } : Montgomery_curve.params)
  : Weierstrass_curve.params
  =
  let open Modulo_ops in
  let a = (of_int 3 - square c_A) / (of_int 3 * square c_B) in
  let b = ((of_int 2 * cubed c_A) - (of_int 9 * c_A)) / (of_int 27 * cubed c_B) in
  let ret = Weierstrass_curve.create_params ~a ~b ~alpha in
  assert (Z.equal ret.twisted_scale twisted_scale);
  ret
;;

let weierstrass_params_to_montgomery_params
  ({ a = _; b = _; alpha; s; twisted_scale } : Weierstrass_curve.params)
  : Montgomery_curve.params
  =
  let open Modulo_ops in
  let c_A = of_int 3 * alpha * s in
  let c_B = s in
  { c_A; c_B; twisted_scale }
;;

let twisted_edwards_affine_to_weierstrass_affine twisted_edwards_params point =
  twisted_edwards_affine_to_montgomery_affine twisted_edwards_params point
  |> Option.map
       ~f:
         (montgomery_affine_to_weierstrass_affine
            (twisted_edwards_params_to_montgomery_params twisted_edwards_params))
;;

let weierstrass_affine_to_twisted_edwards_affine weierstrass_params point =
  let montgomery_params = weierstrass_params_to_montgomery_params weierstrass_params in
  montgomery_affine_to_twisted_edwards_affine
    montgomery_params
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
