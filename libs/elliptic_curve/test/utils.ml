include Field_ops_test.Utils
module Point = Elliptic_curve_lib.Point

let affine_to_jacobian p point =
  let open Z in
  let z = random_z ~lo_incl:one ~hi_incl:(p - one) in
  let x = modulo_multiply ~p (Ark_bls12_377_g1.x point) (pow z 2) in
  let y = modulo_multiply ~p (Ark_bls12_377_g1.y point) (pow z 3) in
  { Point.Jacobian.x; y; z }
;;

let jacobian_to_affine p ({ Point.Jacobian.x; y; z } : Z.t Point.Jacobian.t) =
  let x = modulo_multiply ~p x (modulo_inverse ~p Z.(z ** 2)) in
  let y = modulo_multiply ~p y (modulo_inverse ~p Z.(z ** 3)) in
  (* TODO(fyquah): Don't assume the test outputs are always non-infinity. *)
  Ark_bls12_377_g1.create ~x ~y ~infinity:false
;;
