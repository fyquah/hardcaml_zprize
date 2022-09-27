(** Wraps the actual controller to create one that can utilize a fully pipelined
    adder. *)

open Hardcaml

module Make (Config : sig
  module Top_config : Config.S

  val pipeline_depth : int
  val input_point_bits : int
end) : sig
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

  module O' : sig
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

  val ctrl0_windows : int
  val ctrl1_windows : int
  val ctrl2_windows : int

  module O : sig
    type 'a t = { q : 'a O'.t array } [@@deriving sexp_of, hardcaml]
  end

  val hierarchical : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
end
