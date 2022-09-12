open! Base
open! Hardcaml
open! Bits

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
    Bits.to_z ~signedness:Unsigned t |> Z.to_string |> [%sexp_of: String.t]
  ;;

  let num_bits = 64
  let zero = of_int ~width:num_bits 0
  let one = of_int ~width:num_bits 1
  let two = of_int ~width:num_bits 2
  let epsilon = of_int ~width:num_bits ((1 lsl 32) - 1)
  let modulus_z = Gf_z.to_z Gf_z.modulus
  let modulus = of_z ~width:num_bits modulus_z
  let is_normalized x = Uop.(x <: modulus)
  let to_canonical x = mux2 (is_normalized x) x (x -: modulus)
  let of_z z = Z.erem z modulus_z |> Bits.of_z ~width:64
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
    let res = left *: right |> pipe in
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
