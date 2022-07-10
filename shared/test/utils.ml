open Core
module Extended_euclidean = Snarks_r_fun.Extended_euclidean
module Point = Snarks_r_fun.Point

let a_big_prime =
  Z.of_string
    "21888242871839275222246405745257275088696311157297823662689037894645226208583"
;;

let sexp_of_z z = Sexp.Atom ("0x" ^ Z.format "x" z)

let random_z =
  let random = Splittable_random.State.create Random.State.default in
  fun ~lo_incl ~hi_incl ->
    let generate =
      Bigint.gen_incl (Bigint.of_zarith_bigint lo_incl) (Bigint.of_zarith_bigint hi_incl)
    in
    Bigint.to_zarith_bigint (Quickcheck.Generator.generate ~size:1 ~random generate)
;;

let modulo_inverse ~p x =
  let x = Z.( mod ) x p in
  let { Extended_euclidean.coef_x; coef_y = _; gcd } =
    Extended_euclidean.extended_euclidean ~x ~y:p
  in
  assert (Z.equal gcd Z.one);
  let ret = Z.(coef_x mod p) in
  let ret = if Z.lt ret Z.zero then Z.(ret + p) else ret in
  if not (Z.gt ret (Z.of_int (-1)) && Z.lt ret p)
  then raise_s [%message (ret : z) (p : z)];
  ret
;;

let modulo_multiply ~p a b = Z.( mod ) (Z.( * ) a b) p

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
