(** Pippenger pipeline controller. *)

open! Base
open Hardcaml

module Make (Config : Config.S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; scalar : 'a array
      ; scalar_valid : 'a
      ; last_scalar : 'a
      ; affine_point : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { done_ : 'a
      ; scalar_read : 'a
      ; window : 'a
      ; bucket : 'a
      ; adder_affine_point : 'a
      ; bubble : 'a
      ; execute : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module State : sig
    type t

    val names : string list
  end

  val create : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
  val hierarchy : Scope.t -> Signal.t Interface.Create_fn(I)(O).t

  module For_synthesis : sig
    val create : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
  end
end
