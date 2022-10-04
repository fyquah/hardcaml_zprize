(** Presently unused module to compute modulo square of two numbers. *)

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
  -> Signal.t
  -> Signal.t

module With_interface (M : sig
  val bits : int
end) : sig
  val bits : int

  module I : sig
    type 'a t =
      { clock : 'a
      ; enable : 'a
      ; x : 'a
      ; in_valid : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { y : 'a
      ; out_valid : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : config:Config.t -> Scope.t -> Signal.t I.t -> Signal.t O.t
end
