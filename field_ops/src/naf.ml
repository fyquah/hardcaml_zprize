open Core
open Hardcaml

module Bit = struct
  type t =
    | Zero
    | Pos_one
    | Neg_one
  [@@deriving sexp_of]

  let of_int = function
    | 0 -> Zero
    | 1 -> Pos_one
    | -1 -> Neg_one
    | _ -> assert false
  ;;
end

type t = Bit.t array [@@deriving sexp_of]

let width t = Array.length t - 1

let of_bits e =
  (* Adapted from https://en.wikipedia.org/wiki/Non-adjacent_form *)
  let open Bits in
  let z = Array.create ~len:(Bits.width e + 1) Bit.Zero in
  let e = ref e in
  let i = ref 0 in
  while is_vdd (!e >:. 0) do
    if is_vdd !e.:(0)
    then (
      let zi = 2 - Bits.to_int (Bits.sel_bottom !e 2) in
      e := !e -:. zi;
      z.(!i) <- Bit.of_int zi)
    else z.(!i) <- Zero;
    e := Bits.srl !e 1;
    i := !i + 1
  done;
  z
;;

let to_bits (t : t) =
  let width = width t in
  Array.foldi t ~init:(Bits.zero width) ~f:(fun i unchanged bit ->
      let x = Bits.sll (Bits.one width) i in
      match bit with
      | Zero -> unchanged
      | Pos_one -> Bits.(unchanged +: x)
      | Neg_one -> Bits.(unchanged -: x))
;;

let bits_lsb t = Array.to_list t
