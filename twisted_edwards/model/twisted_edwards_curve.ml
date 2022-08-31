open! Base
open Bls12_377_util

type params =
  { a : z
  ; d : z
  ; twisted_scale : z
  }
[@@deriving sexp_of]

type affine =
  { x : z
  ; y : z
  }
[@@deriving sexp_of]

type affine_with_t =
  { x : Z.t
  ; y : Z.t
  ; t : Z.t
  }

type extended =
  { x : z
  ; y : z
  ; z : z
  ; t : z
  }
[@@deriving sexp_of]

let affine_identity = { x = Z.zero; y = Z.one }

let affine_to_extended ~z ({ x; y } : affine) : extended =
  { x = modulo_mult x z; y = modulo_mult y z; z; t = modulo_mult (modulo_mult x y) z }
;;

let affine_to_affine_with_t ({ x; y } : affine) : affine_with_t =
  { x; y; t = modulo_mult x y }
;;

let affine_neg ({ x; y } : affine) : affine = { x; y = modulo_neg y }

let extended_to_affine { x; y; z; t } : affine =
  let open Modulo_ops in
  assert (equal (x / z * (y / z)) (t / z));
  { x = x / z; y = y / z }
;;

let _add_not_equal
  { a; d = _; twisted_scale = _ }
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
  { a = _; d; twisted_scale = _ }
  ({ x = x1; y = y1; z = z1; t = t1 } : extended)
  ({ x = x2; y = y2; t = t2 } : affine_with_t)
  : extended
  =
  (* https://hyperelliptic.org/EFD/g1p/auto-twisted-extended-1.html#addition-madd-2008-hwcd-3 *)
  let open Modulo_ops in
  let c_A = (y1 - x1) * (y2 - x2) in
  let c_B = (y1 + x1) * (y2 + x2) in
  let c_C = t1 * of_int 2 * d * t2 in
  let c_D = of_int 2 * z1 in
  let c_E = c_B - c_A in
  let c_F = c_D - c_C in
  let c_G = c_D + c_C in
  let c_H = c_B + c_A in
  let x3 = c_E * c_F in
  let y3 = c_G * c_H in
  let t3 = c_E * c_H in
  let z3 = c_F * c_G in
  { x = x3; y = y3; z = z3; t = t3 }
;;

(*
 * host precomputed optimization ------
 *  - Represent host point (x,y,t) as (y-x,y+x,4dt)
 *  - Represent fpga point (x,y,z,t) as (2x,2y,4z,t)
 * Then, we get rid of all the constants from the equations (below)
 *)

(* not a real projective representation - xy != t *)
let affine_with_t_to_host_extended_representation
  ({ d; _ } : params)
  ({ x; y; t } : affine_with_t)
  : extended
  =
  let open Modulo_ops in
  let x_host = (y - x) / of_int 2 in
  let y_host = (y + x) / of_int 2 in
  let t_host = of_int 4 * d * t in
  { x = x_host; y = y_host; t = t_host; z = of_int 1 }
;;

let affine_to_host_extended_representation params affine : extended =
  affine_with_t_to_host_extended_representation params (affine_to_affine_with_t affine)
;;

let affine_to_host_affine_with_t_representation params affine : affine_with_t =
  let { x; y; t; _ } = affine_to_host_extended_representation params affine in
  { x; y; t }
;;

(* bucket representation *)
let to_fpga_bucket_representation ({ x; y; z; t } : extended) : extended =
  let open Modulo_ops in
  { x = of_int 2 * (y - x); y = of_int 2 * (y + x); z = of_int 4 * z; t }
;;

let affine_to_fpga_bucket_representation ~z (p : affine) : extended =
  to_fpga_bucket_representation (affine_to_extended ~z p)
;;

let from_fpga_bucket_representation ({ x; y; z; t } : extended) : extended =
  let open Modulo_ops in
  let y_minus_x = x / of_int 2 in
  let y_plus_x = y / of_int 2 in
  let x = (y_plus_x - y_minus_x) / of_int 2 in
  let y = (y_minus_x + y_plus_x) / of_int 2 in
  { x; y; z = z / of_int 4; t }
;;

let fpga_bucket_representation_to_affine (p : extended) : affine =
  extended_to_affine (from_fpga_bucket_representation p)
;;

(* running representation *)
let to_fpga_running_representation ({ d; _ } : params) ({ x; y; z; t } : extended)
  : extended
  =
  let open Modulo_ops in
  let t1 = modulo_square_root (d / of_int 4) in
  { x = t1 * (y - x); y = t1 * (y + x); z = z / of_int 4; t = d * t }
;;

let affine_to_fpga_running_representation ~z params (p : affine) : extended =
  to_fpga_running_representation params (affine_to_extended ~z p)
;;

let from_fpga_running_representation ({ d; _ } : params) ({ x; y; z; t } : extended)
  : extended
  =
  let open Modulo_ops in
  let t1 = modulo_square_root (d / of_int 4) in
  let y_minus_x = x / t1 in
  let y_plus_x = y / t1 in
  let x = (y_plus_x - y_minus_x) / of_int 2 in
  let y = (y_minus_x + y_plus_x) / of_int 2 in
  { x; y; z = z * of_int 4; t = t / d }
;;

let fpga_running_representation_to_affine params (p : extended) : affine =
  extended_to_affine (from_fpga_running_representation params p)
;;

(* (2x1,2y1,4z1,t1) + ((y2-x2)/2,(y2+x2)/2,4d*t2) -> (2x3,2y3,4z3,t3) *)
let add_unified_precomputed
  ({ x = x1; y = y1; z = z1; t = t1 } : extended)
  ({ x = x_host; y = y_host; t = t_host } : affine_with_t)
  : extended
  =
  let open Modulo_ops in
  let c_A = x1 * x_host in
  let c_B = y1 * y_host in
  let c_C = t1 * t_host in
  let c_D = z1 in
  let c_E = c_B - c_A in
  let c_F = c_D - c_C in
  let c_G = c_D + c_C in
  let c_H = c_B + c_A in
  let x3 = (c_G * c_H) - (c_E * c_F) in
  let y3 = (c_G * c_H) + (c_E * c_F) in
  let t3 = c_E * c_H in
  let z3 = c_F * c_G in
  { x = x3; y = y3; z = z3; t = t3 }
;;

let add_full
  ({ x = x1; y = y1; z = z1; t = t1 } : extended)
  ({ x = x2; y = y2; t = t2; z = z2 } : extended)
  : extended
  =
  let open Modulo_ops in
  let c_A = x1 * x2 in
  let c_B = y1 * y2 in
  let c_C = t1 * t2 in
  let c_D = z1 * z2 in
  let c_E = c_B - c_A in
  let c_F = c_D - c_C in
  let c_G = c_D + c_C in
  let c_H = c_B + c_A in
  let x3 = (c_G * c_H) - (c_E * c_F) in
  let y3 = (c_G * c_H) + (c_E * c_F) in
  let t3 = c_E * c_H in
  let z3 = c_F * c_G in
  { x = x3; y = y3; z = z3; t = t3 }
;;
