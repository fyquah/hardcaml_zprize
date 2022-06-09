(** Computes ([a + b mod P]) over multiple pipeline stages.  *)

open Hardcaml

val latency : stages: int -> int

val hierarchical
  : scope: Scope.t
  -> clock: Signal.t
  -> enable: Signal.t
  -> stages: int
  -> p: Signal.t
  -> Signal.t
  -> Signal.t
  -> Signal.t

module With_interface(M : sig val bits : int end) : sig
  val bits : int

  module I : sig
    type 'a t =
      { clock : 'a
      ; enable : 'a
      ; p : 'a
      ; x : 'a
      ; y : 'a
      ; valid : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { z : 'a
      ; valid : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : stages: int -> Scope.t -> Signal.t I.t -> Signal.t O.t
end
