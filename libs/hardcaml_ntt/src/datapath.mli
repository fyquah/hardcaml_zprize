open! Base
open! Hardcaml

module Make (Config : Core_config.S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; first_iter : 'a
      ; d1 : 'a
      ; d2 : 'a
      ; omegas : 'a list
      ; start_twiddles : 'a
      ; twiddle_stage : 'a
      ; twiddle_update : 'a Twiddle_update.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { q1 : 'a
      ; q2 : 'a
      ; twiddle_update_q : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : ?row:int -> Scope.t -> Signal.t Interface.Create_fn(I)(O).t
  val hierarchy : ?row:int -> Scope.t -> Signal.t Interface.Create_fn(I)(O).t
end
