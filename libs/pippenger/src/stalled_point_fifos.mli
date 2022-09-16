open! Base
open Hardcaml

module Make (Config : Config.S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; push : 'a
      ; scalar : 'a [@bits window_size_bits]
      ; window : 'a [@bits log_num_windows]
      ; affine_point : 'a [@bits affine_point_bits]
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
      ; affine_point_out : 'a [@bits affine_point_bits]
      ; scalar_out : 'a [@bits window_size_bits]
      ; scalar_out_valid : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
  val hierarchy : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
end
