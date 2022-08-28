(** After reset, takes in a stream of scalars and affine points to compute MSM
    on.

    After processing has finished, will return a stream of [num_buckets *
    num_windows] back to the host, with buckets in high -> low order.

    Currently only supports a single controller, but later will be modified to
    support a multi-SLR parallel solution.
*)

open Hardcaml

module Make (Config : Config.S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; scalar : 'a
      ; scalar_valid : 'a
      ; last_scalar : 'a
      ; input_point : 'a
      ; result_point_ready : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { result_point : 'a
      ; result_point_valid : 'a
      ; scalar_and_input_point_ready : 'a
      ; error : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module State : sig
    type t

    val names : string list
  end

  val hierarchical
    :  build_mode:Build_mode.t
    -> Scope.t
    -> Signal.t Interface.Create_fn(I)(O).t
end
