open Hardcaml

(** For running this through a SAT solver. *)
module Make_comb_implementation(Comb : Comb.S) : sig
  val create
    : stages: int
    -> pipe: (n: int -> Comb.t -> Comb.t)
    -> Comb.t list
    -> Comb.t
end

val hierarchical
  : scope: Scope.t
  -> clock: Signal.t
  -> enable: Signal.t
  -> stages: int
  -> Signal.t list
  -> Signal.t
