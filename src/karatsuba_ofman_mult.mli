open Hardcaml

(** Returns the latency of a multiplier of the given [depth]. *)
val latency : depth: int -> int

val create
  : ?enable: Signal.t
  -> depth: int
  -> scope: Scope.t
  -> clock:Signal.t
  -> Signal.t
  -> Signal.t
  -> Signal.t

val hierarchical
  : enable: Signal.t
  -> depth: int
  -> scope: Scope.t
  -> clock:Signal.t
  -> Signal.t
  -> Signal.t
  -> Signal.t

module With_interface(M : sig
    val num_bits : int
    val depth : int
  end) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; enable : 'a
      ; valid : 'a
      ; a : 'a
      ; b : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { c : 'a
      ; valid : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : Scope.t -> Signal.t I.t -> Signal.t O.t
end
