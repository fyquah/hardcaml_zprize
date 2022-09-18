open Base
open Hardcaml
module Zarith = Z

module type S = Gf_intf.S

module Z = struct
  type t = Z.t [@@deriving compare, equal]

  let sexp_of_t t = Z.to_string t |> [%sexp_of: String.t]

  module Hex = struct
    open Hardcaml

    type nonrec t = t

    let sexp_of_t t =
      Bits.of_z ~width:64 t
      |> Bits.to_constant
      |> Constant.to_hex_string ~signedness:Unsigned
      |> String.sexp_of_t
    ;;
  end

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

  let rec random =
    let p = Z.(two ** 32) in
    fun () ->
      let a = Random.int (1 lsl 32) |> Z.of_int in
      let b = Random.int (1 lsl 32) |> Z.of_int in
      let c = Z.(a + (b * p)) in
      if Z.compare c modulus < 0 then c else random ()
  ;;
end

module Make (Bits : Comb.S) = struct
  open Bits

  type nonrec t = t

  module Hex = struct
    type nonrec t = t

    let sexp_of_t t =
      Bits.to_constant t
      |> Constant.to_hex_string ~signedness:Unsigned
      |> String.sexp_of_t
    ;;
  end

  let compare a b = if equal (a <: b) vdd then -1 else if equal (a >: b) vdd then 1 else 0
  let equal a b = equal (a ==: b) vdd

  let sexp_of_t t =
    Bits.to_z ~signedness:Unsigned t |> Zarith.to_string |> [%sexp_of: String.t]
  ;;

  let num_bits = 64
  let zero = of_int ~width:num_bits 0
  let one = of_int ~width:num_bits 1
  let two = of_int ~width:num_bits 2
  let epsilon = of_int ~width:num_bits ((1 lsl 32) - 1)
  let modulus_z = Z.to_z Z.modulus
  let modulus = of_z ~width:num_bits modulus_z
  let is_normalized x = Uop.(x <: modulus)
  let to_canonical x = mux2 (is_normalized x) x (x -: modulus)
  let of_z z = Zarith.erem z modulus_z |> Bits.of_z ~width:64
  let to_z = Bits.to_z ~signedness:Unsigned
  let overflow res = mux2 (msb res) Uop.(lsbs res +: epsilon) res

  let _add left right =
    let res = Uop.(left +: right) in
    overflow res |> overflow |> lsbs
  ;;

  let add left right =
    let res = Uop.(left +: right) in
    mux2 (res >=: ue modulus) (res -: ue modulus) res |> lsbs
  ;;

  let underflow res = mux2 (msb res) Uop.(lsbs res -: epsilon) res

  let _sub left right =
    let res = Uop.(left -: right) in
    underflow res |> underflow |> lsbs
  ;;

  let sub left right =
    let res = Uop.(left -: right) in
    mux2 (msb res) (res +: ue modulus) res |> lsbs
  ;;

  let mul ?(pipe = Fn.id) left right =
    let res = left *: right |> pipe |> pipe |> pipe in
    let t0 = Uop.(res.:[63, 0] -: res.:[127, 96]) in
    let t0 = underflow t0 |> pipe in
    let t1 =
      let r = res.:[95, 64] in
      Uop.((r @: Bits.zero 32) -: r) |> pipe
    in
    let final = t0 +: t1 in
    mux2 (final >=: ue modulus) (final -: ue modulus) final |> lsbs |> pipe
  ;;

  let to_bits t = t
  let of_bits t = t
  let negate x = mux2 (x ==:. 0) x (modulus -: to_canonical x)
  let ( + ) = add
  let ( - ) = sub
  let ( * ) a b = mul a b
end

module Bits = Make (Bits)
module Signal = Make (Signal)
