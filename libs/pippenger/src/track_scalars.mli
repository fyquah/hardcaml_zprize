open! Base
open! Hardcaml

module Make (Config : Config.S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; scalar : 'a
      ; bubble : 'a
      ; shift : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { is_in_pipeline : 'a
      ; scalar_out : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
  val hierarchy : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
end
