open! Base
open! Hardcaml

module Make (Config : Config.S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; scalar_in : 'a [@bits window_size_bits]
      ; shift : 'a
      ; scalar_match : 'a [@bits window_size_bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t = { is_in_pipeline : 'a } [@@deriving sexp_of, hardcaml]
  end

  val create : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
  val hierarchy : window:int -> Scope.t -> Signal.t Interface.Create_fn(I)(O).t
end
