(** Internal module to represent constants in {{: https://en.wikipedia.org/wiki/Non-adjacent_form} Non Adjacent Form} *)

open Hardcaml

module Bit : sig
  type t =
    | Zero
    | Pos_one
    | Neg_one
end

type t [@@deriving sexp_of]

val width : t -> int
val of_bits : Bits.t -> t
val to_bits : t -> Bits.t
val bits_lsb : t -> Bit.t list
val hamming_weight : t -> int
