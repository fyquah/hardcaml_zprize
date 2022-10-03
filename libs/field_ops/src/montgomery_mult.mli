(** Computes [z' = x' * y' mod p], where p is a prime, where x', y' and z' are x, y and z in montgomery space.

    This module is not presently maintained.
*)

open! Hardcaml

module Config : sig
  type t =
    { multiplier_config :
        [ `Squarer of Squarer.Config.t | `Multiplier of Karatsuba_ofman_mult.Config.t ]
    ; montgomery_reduction_config : Montgomery_reduction.Config.t
    }

  val latency : t -> int
end

module With_interface (M : sig
  val bits : int
end) : sig
  module Config = Config

  val bits : int

  module I : sig
    type 'a t =
      { clock : 'a
      ; enable : 'a
      ; x : 'a
      ; y : 'a
      ; valid : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { z : 'a
      ; valid : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : config:Config.t -> p:Z.t -> Scope.t -> Signal.t I.t -> Signal.t O.t
end
