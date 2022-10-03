(** Computes the least significant half of the product of two operands. *)

(** Algorithm as follows:

    1. Given two numbers x and y, represent them as [x1 + (2^k) * x0] and
       [y1 + (2^k) * x0]. k is equal to [ceil(width(x) /2)] where [width(x) = width(y)]

    2. The product of these two numbers can be written as follows

    {[
      (x1 + (2^k) * x0) * (y1 + (2^k)) * y0
      = (x1y1 * (2^2k)) + ((x0y1 + x1y0) * (2^k)) + x0y0
    ]}

    3. As we are computing an bottom half, we drop the [x1y1 * (2^2k)] term
    [x0y0] is computed with a full multiplier (this implementation uses the
    karatsuba-ofman multiplier). The middle two terms ([x0y1] and [x1y0]) are
    recursively computed with [half_width_multiply]

    4. The base case of the recursion is implemented using a ground multiplier.

    Our implementation support both 2-part splitting ([Radix_2]) as well as
    3-part splitting ([Radix_3]). 3-part splitting is similar to the above,
    but breaks the terms to be multiplied into 3 parts.
*)

open Base
open Hardcaml

module Config : sig
  module Level = Karatsuba_ofman_mult.Config.Level

  type t =
    { levels : Level.t list
    ; ground_multiplier : Ground_multiplier.Config.t
    }

  val latency : t -> int
end

module Input : sig
  type t =
    | Multiply_by_constant of (Signal.t * Bits.t)
    | Multiply of (Signal.t * Signal.t)
    | Square of Signal.t
end

val hierarchical
  :  ?name:string
  -> config:Config.t
  -> clock:Signal.t
  -> enable:Signal.t
  -> scope:Scope.t
  -> Multiplier_input.t
  -> Signal.t

module With_interface_multiply (M : sig
  val bits : int
end) : sig
  val bits : int

  module I : sig
    type 'a t =
      { clock : 'a
      ; enable : 'a
      ; x : 'a
      ; y : 'a
      ; in_valid : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { z : 'a
      ; out_valid : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : config:Config.t -> Scope.t -> Signal.t I.t -> Signal.t O.t
end
