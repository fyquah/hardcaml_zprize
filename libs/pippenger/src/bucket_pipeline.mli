open! Base
open! Hardcaml

module Core (Config : Config.S) (Scalar_config : Scalar.Scalar_config.S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; scalar_in : 'a
      ; shift : 'a
      ; scalar_match : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t = { is_in_pipeline : 'a } [@@deriving sexp_of, hardcaml]
  end

  val create : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
  val hierarchy : window:int -> Scope.t -> Signal.t Interface.Create_fn(I)(O).t
end

module Make (Config : Config.S) (Scalar_config : Scalar.Scalar_config.S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; window : 'a
      ; scalar_in : 'a array
      ; stalled_scalar : 'a
      ; stalled_scalar_valid : 'a
      ; process_stalled : 'a
      ; bubble : 'a
      ; shift : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t = { is_in_pipeline : 'a list } [@@deriving sexp_of, hardcaml]
  end

  val create : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
  val hierarchy : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
end
