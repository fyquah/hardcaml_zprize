open Base

type t = Z.t [@@deriving compare, equal]

let sexp_of_t t = Z.to_string t |> [%sexp_of: String.t]

(* This is a solinas prime (2^64 - 2^32 + 1) which has a fast modular
     reduction algorithm. *)
let modulus = Z.((one lsl 64) - (one lsl 32) + one)
let zero = Z.zero
let one = Z.one
let two = Z.of_int 2
let of_z t = Z.(erem t modulus)
let add a b = Z.((a + b) mod modulus)
let sub a b = Z.(erem (a - b) modulus)
let mul a b = Z.(a * b mod modulus)
let negate a = if Z.(equal a zero) then Z.zero else Z.(modulus - a)
let ( + ) = add
let ( - ) = sub
let ( * ) = mul
let to_z t = t

(* Inverse computed by the extended euclidian algorithm *)
let inverse a =
  let rec f t nt r nr =
    if Z.equal nr Z.zero
    then t, r
    else (
      let q = Z.div r nr in
      let t = nt
      and nt = Z.(t - (q * nt)) in
      let r = nr
      and nr = Z.(r - (q * nr)) in
      f t nt r nr)
  in
  let t, r = f Z.zero Z.one modulus a in
  if Z.compare r Z.one > 0 then raise_s [%message "Not invertable" (a : t)];
  if Z.compare t Z.zero < 0 then Z.(t + modulus) else t
;;

let rec pow a n =
  if n < 0
  then raise_s [%message "pow must be raised to positive power" (n : int)]
  else if n = 0
  then Z.one
  else if n = 1
  then a
  else (
    (* recursively divide in half.  We could be a little more efficient still by memoising. *)
    let nl = n / 2 in
    let nr = Int.(n - nl) in
    pow a nl * pow a nr)
;;

(* a * pow a Int.(n - 1) *)

let pp fmt z = Caml.Format.fprintf fmt "%s" (sexp_of_t z |> Sexplib.Sexp.to_string)
