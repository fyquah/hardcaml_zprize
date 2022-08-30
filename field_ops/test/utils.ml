open Core
module Extended_euclidean = Field_ops_lib.Extended_euclidean

let a_big_prime =
  Z.of_string
    "21888242871839275222246405745257275088696311157297823662689037894645226208583"
;;

let sexp_of_z z = Sexp.Atom ("0x" ^ Z.format "x" z)

let generate_z ~lo_incl ~hi_incl =
  Quickcheck.Generator.map
    ~f:Bigint.to_zarith_bigint
    (Bigint.gen_incl (Bigint.of_zarith_bigint lo_incl) (Bigint.of_zarith_bigint hi_incl))
;;

let random_z =
  let random = Splittable_random.State.create Random.State.default in
  fun ~lo_incl ~hi_incl ->
    let generate = generate_z ~lo_incl ~hi_incl in
    Quickcheck.Generator.generate ~size:1 ~random generate
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
