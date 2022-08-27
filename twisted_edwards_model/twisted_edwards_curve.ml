open! Base
open Util

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
