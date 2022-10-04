(** Computes the most significant half of the product of two operands with under approximation. *)

(** The exact error of the approximation is dependent on the config provided.

    The radix-2 approx msb multiplier computes the approximation as follows.

    1. Given two numbers x and y, represent them as [x1 + (2^k) * x0] and
       [y1 + (2^k) * x0]. The exact value of [k] is determined from the
       provided config. (Larger [k] will yield a larger approximation error)

    2. The product of these two numbers can be written as follows

    [(x1 + (2^k) * x0) * (y1 + (2^k) * y0 = (x1y1 * 2^2k) + ((x0y1 + x1y0) * (2^k)) + x0y0]

    3. As we are computing an approximation, we drop the last term [x0y0].
    The first term [x1y1] is computed with a full multiplier (this
    implementation uses the karatsuba-ofman multiplier). The middle two terms
    ([x0y1] and [x1y0]) are recursively computed with the
    [Approx_msb_multiplier]

    4. The base case of the recursion is implemented using a ground multiplier.

    This algorithm is largely similar to those of {!Half_width_multiplier},
    except it uses the upper half and is not exact.

    Our implementation support both 2-part splitting ([Radix_2]) as well as
    3-part splitting ([Radix_3]). 3-part splitting is similar to the above,
    but breaks the terms to be multiplied into 3 parts.

    As mentioned, the exact upper-bound under-approximation is dependent on
    the provided [k]. Please refer to the source of {!Field_ops_model}
    for expect tests that demonstrates this calculation.
 *)

open Base
open Hardcaml

module Config : sig
  module Level : sig
    type t =
      { k : int -> int (** [k] is the amount of shifting to be used in step(1) above. *)
      ; for_karatsuba : Karatsuba_ofman_mult.Config.Level.t
      }
  end

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
