(** Pippenger pipeline controller. *)

open! Base
open Hardcaml

module type Config = sig
  val window_size_bits : int
  val num_windows : int
  val affine_point_bits : int
  val pipeline_depth : int
end

module Make (Config : Config) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; scalar : 'a array
      ; scalar_valid : 'a
      ; affine_point : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { done_ : 'a
      ; scalar_read : 'a
      ; bucket : 'a
      ; window : 'a
      ; adder_affine_point : 'a
      ; bubble : 'a
      ; execute : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
end
