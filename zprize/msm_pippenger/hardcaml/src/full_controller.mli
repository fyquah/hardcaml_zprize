(** Wraps multiple individual pippenger controllers with helper logic and FIFOs
    to create one that can utilize a fully pipelined adder. *)

open Hardcaml

module Make (Config : sig
  module Top_config : Config.S

  val pipeline_depth : int
  val input_point_bits : int
end) : sig
  module Scalar_config : Pippenger.Scalar.Scalar_config.S
  module Scalar : module type of Pippenger.Scalar.Make (Scalar_config)

  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; scalar : 'a Scalar.t array
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
      ; bucket : 'a Scalar.t
      ; adder_affine_point : 'a
      ; bubble : 'a
      ; execute : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val hierarchical
    :  build_mode:Build_mode.t
    -> Scope.t
    -> Signal.t Interface.Create_fn(I)(O).t
end
