(** Fully unrolled eliptic curve point doubling. *)

open Hardcaml

module Config : sig
  open Signal

  type fn =
    { latency : int
    ; impl : scope:Scope.t -> clock:t -> enable:t -> t -> t option -> t
    }

  type t =
    { fp_multiply : fn
          (** [fp_multiply] is the implementation of a finite-field multiplication.
     * The output of has to be between 0 and p-1 inclusive.
    *)
    ; fp_square : fn
    ; p : Z.t (** [p] is the modulus prime. *)
    }
end

(** Jacobian representation of a point *)
module Jacobian : sig
  type 'a t =
    { x : 'a
    ; y : 'a
    ; z : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

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
    type 'a t =
      { valid_out : 'a
      ; data_out : 'a Jacobian.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : config:Config.t -> Scope.t -> Signal.t I.t -> Signal.t O.t
end
