(** Fully unrolled eliptic curve point doubling. *)

open Hardcaml
module Jacobian = Point.Jacobian
module Config = Ec_fpn_ops_config

(** Latency of the eliptic curve point doubling datapath. *)
val latency : Config.t -> int

module With_interface (M : sig
  val bits : int
end) : sig
  val bits : int
  val latency : Config.t -> int

  module I : sig
    type 'a t =
      { clock : 'a
      ; enable : 'a
      ; valid_in : 'a
      ; data_in : 'a Jacobian.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    module Data : sig
      type 'a t =
        { point : 'a Jacobian.t
        ; z_squared : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    type 'a t =
      { ready_in : 'a
      ; valid_out : 'a
      ; data_out : 'a Data.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : config:Config.t -> Scope.t -> Signal.t I.t -> Signal.t O.t
end
