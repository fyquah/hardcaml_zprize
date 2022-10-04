(** Computes ([a + b mod P]) over multiple pipeline stages. 

    This module requires a < P and b < P. Functionally, it implements the
    following over multiple pipeline stages.

    {[
      mux2 ((a + b) >= P)
        ((a + b) - P)
        (a + b)
    ]}
*)

open Hardcaml

val latency : stages:int -> int

val hierarchical
  :  scope:Scope.t
  -> clock:Signal.t
  -> enable:Signal.t
  -> stages:int
  -> p:Z.t
  -> Signal.t
  -> Signal.t
  -> Signal.t

module With_interface (M : sig
  val bits : int
end) : sig
  val bits : int

  module I : sig
    type 'a t =
      { clock : 'a
      ; enable : 'a
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

  val create : stages:int -> p:Z.t -> Scope.t -> Signal.t I.t -> Signal.t O.t
end
