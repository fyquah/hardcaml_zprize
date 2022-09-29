open! Base
open Hardcaml

module Make (Config : Config.S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; push : 'a
      ; scalar : 'a
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
      ; scalar_out : 'a
      ; scalar_out_valid : 'a
      ; scalars_out : 'a array
      ; scalars_out_valid : 'a array
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
  val hierarchy : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
end
