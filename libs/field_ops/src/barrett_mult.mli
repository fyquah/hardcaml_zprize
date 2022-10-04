(** Modulo multiplication with barrett reduction. *)

(** Please see the documentation in {!Barrett_reduction} for more
   information.
*)

open Base
open Hardcaml

module Config : sig
  type t =
    { multiplier_config : Karatsuba_ofman_mult.Config.t
    ; barrett_reduction_config : Barrett_reduction.Config.t
    }
end

module With_interface (M : sig
  val bits : int
end) : sig
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
