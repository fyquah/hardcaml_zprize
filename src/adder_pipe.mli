(** Multistage pipelined ripple-carry-adder. *)

open Hardcaml

val hierarchical
  : scope: Scope.t
  -> clock: Signal.t
  -> enable: Signal.t
  -> stages: int
  -> Signal.t list
  -> Signal.t

module For_testing : sig
  (** A combinationa implementation for writing proofs. *)
  val create_combinational
    : (module Comb.S with type t = 'a)
    -> stages: int
    -> 'a list
    -> 'a 
end
