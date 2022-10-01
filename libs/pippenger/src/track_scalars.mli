open! Base
open! Hardcaml

module Make (Config : Config.S) (Scalar_config : Scalar.Scalar_config.S) : sig
  module Scalar : module type of Scalar.Make (Scalar_config)

  module I : sig
    type 'a t =
      { clock : 'a
      ; scalar : 'a Scalar.t
      ; bubble : 'a
      ; shift : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { is_in_pipeline : 'a
      ; scalar_out : 'a Scalar.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
  val hierarchy : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
end
