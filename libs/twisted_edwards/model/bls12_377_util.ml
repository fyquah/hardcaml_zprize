open Core
module Extended_euclidean = Field_ops_lib.Extended_euclidean

type z = Z.t [@@deriving equal]

let sexp_of_z z = Sexp.Atom ("0x" ^ Z.format "x" z)
let half x = Z.div x (Z.of_int 2)
let p = Ark_bls12_377_g1.modulus ()
let modulo_mult a b = Z.(a * b mod p)
let modulo_neg a = Z.((neg a + p) mod p)

let modulo_inverse x =
  let x = Z.( mod ) x p in
  let { Extended_euclidean.coef_x; coef_y = _; gcd } =
    Extended_euclidean.extended_euclidean ~x ~y:p
  in
  if not (Z.equal gcd Z.one) then raise_s [%message "Error computing modulo inverse!"];
  let ret = Z.(coef_x mod p) in
  let ret = if Z.lt ret Z.zero then Z.(ret + p) else ret in
  if not (Z.gt ret (Z.of_int (-1)) && Z.lt ret p)
  then raise_s [%message (ret : z) (p : z)];
  Z.((ret + p) mod p)
;;

let modulo_sub a b = Z.((a - b + p) mod p)
let modulo_add a b = Z.((a + b) mod p)
let modulo_div a b = Z.((modulo_mult a (modulo_inverse b) + p) mod p)

let rec modulo_pow base exponent =
  if Z.equal exponent Z.zero
  then Z.one
  else if Z.equal exponent Z.one
  then base
  else (
    let is_exponent_odd = Z.equal Z.one (Z.( land ) Z.one exponent) in
    let partial = modulo_pow base (half exponent) in
    if is_exponent_odd
    then modulo_mult (modulo_mult partial partial) base
    else modulo_mult partial partial)
;;

let modulo_square_root a = snd (Option.value_exn (Toneli_shank.tonelli_shank ~p a))

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

module Modulo_ops = struct
  include Z

  let ( * ) = modulo_mult
  let ( - ) = modulo_sub
  let ( + ) = modulo_add
  let ( / ) = modulo_div
end

let cubed x =
  let open Modulo_ops in
  x * x * x
;;

let square x =
  let open Modulo_ops in
  x * x
;;
