open! Base
open! Hardcaml

module Make (Config : Core_config.S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; input_done : 'a
      ; output_done : 'a
      ; cores_done : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { done_ : 'a
      ; start_input : 'a
      ; start_output : 'a
      ; start_cores : 'a
      ; first_iter : 'a
      ; flip : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
  val hierarchy : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
end
