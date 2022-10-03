open! Base
open Hardcaml

module Make (Config : Config.S) (Scalar_config : Scalar.Scalar_config.S) : sig
  module Scalar : module type of Scalar.Make (Scalar_config)

  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; push : 'a
      ; scalar : 'a Scalar.t
      ; window : 'a
      ; affine_point : 'a
      ; pop : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { all_windows_have_stall : 'a
      ; some_windows_are_full : 'a
      ; all_windows_are_empty : 'a
      ; current_window_has_stall : 'a
      ; affine_point_out : 'a
      ; scalar_out : 'a Scalar.t
      ; scalar_out_valid : 'a
      ; scalars_out : 'a Scalar.t array
      ; scalars_out_valid : 'a array
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : build_mode:Build_mode.t -> Scope.t -> Signal.t Interface.Create_fn(I)(O).t

  val hierarchy
    :  build_mode:Build_mode.t
    -> Scope.t
    -> Signal.t Interface.Create_fn(I)(O).t
end
