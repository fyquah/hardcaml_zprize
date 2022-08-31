(** Fully unrolled eliptic curve point adding.

    Based on the formula here:

    [https://en.wikibooks.org/wiki/Cryptography/Prime_Curve/Jacobian_Coordinates#Point_Addition_(12M_+_4S)]

    Currently only mixed Jacobian and Affine point addition is supported.

    An optimization requires the previous stage provide this module with
   data_in1.z^2 as an input.

    In order to reduce the amount of DSPs used we arbitrate multiplications and
   squares in certain stages. *)

open Hardcaml
open! Point
module Config = Ec_fpn_ops_config

(** Latency of the eliptic curve point adding datapath. *)
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
      ; data_in0 : 'a Affine.t
      ; data_in1 : 'a Jacobian.t
      ; data_in1_z_squared : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { valid_out : 'a
      ; ready_in : 'a
      ; data_out : 'a Jacobian.t
      ; error : 'a
          (** Raised when the input points are the same - the doubler module must be
         used and this result thrown away. *)
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : config:Config.t -> Scope.t -> Signal.t I.t -> Signal.t O.t
end
