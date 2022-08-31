open Core
open Elliptic_curve_lib

let mod' ~p x =
  let open Z in
  if lt x zero then (x mod p) + p else x mod p
;;

let make_multiply_op ~montgomery ~p =
  let open Z in
  let mod_p = mod' ~p in
  if montgomery
  then (
    let r = one lsl log2up p in
    let r' = Field_ops_test.Utils.modulo_inverse ~p r in
    Staged.stage (fun a b -> mod_p (a * b * r')))
  else Staged.stage (fun a b -> mod_p (a * b))
;;

let point_double ~montgomery ~p { Point.Jacobian.x; y; z } =
  let open Z in
  let scale ~by a = of_int by * a in
  let mod_p = mod' ~p in
  let ( * ) = Staged.unstage (make_multiply_op ~montgomery ~p) in
  let y_squared = mod_p (y * y) in
  let x_squared = mod_p (x * x) in
  let s = mod_p (scale ~by:4 (x * y_squared)) in
  let m = mod_p (scale ~by:3 x_squared) in
  let m_squared = m * m in
  let x' = mod_p (m_squared - scale ~by:2 s) in
  let y_pow_4 = mod_p (y_squared * y_squared) in
  let y' = mod_p ((m * (s - x')) - scale ~by:8 y_pow_4) in
  let z' = mod_p (scale ~by:2 (y * z)) in
  { Point.Jacobian.x = x'; y = y'; z = z' }
;;

let mixed_add
    ~montgomery
    ~p
    { Point.Affine.x = x1; y = y1 }
    { Point.Jacobian.x = x2; y = y2; z = z2 }
  =
  let open Z in
  let scale ~by a = of_int by * a in
  let mod_p = mod' ~p in
  let ( * ) = Staged.unstage (make_multiply_op ~montgomery ~p) in
  let z2_squared = mod_p (z2 * z2) in
  let z2_cubed = mod_p (z2 * z2_squared) in
  let u1 = x1 * z2_squared in
  let u2 = x2 in
  let s1 = y1 * z2_cubed in
  let s2 = y2 in
  let h = mod_p (u2 - u1) in
  let r = mod_p (s2 - s1) in
  let r_squared = r * r in
  let h_squared = h * h in
  let h_cubed = h * h_squared in
  let x3 = mod_p (r_squared - h_cubed - scale ~by:2 (u1 * h_squared)) in
  let y3 = mod_p ((r * ((u1 * h_squared) - x3)) - (s1 * h_cubed)) in
  let z3 = h * z2 in
  { Point.Jacobian.x = x3; y = y3; z = z3 }
;;
