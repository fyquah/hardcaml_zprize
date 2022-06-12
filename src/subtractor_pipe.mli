(** Multistage pipelined ripple-carry-subtractor.
 *
 * See adder_pipe.mli for information about the architecture.
*)

(* TODO(fyquah): The code here is pretty similar to adder_pipe.ml. Consider
 * refactoring to share more code.
 *)

open Hardcaml

type ('difference, 'borrows) result =
  { difference : 'difference
  ; borrows    : 'borrows
  }

val hierarchical
  : scope: Scope.t
  -> clock: Signal.t
  -> enable: Signal.t
  -> stages: int
  -> Signal.t
  -> Signal.t
  -> (Signal.t, Signal.t) result

val hierarchical_general
  : scope: Scope.t
  -> clock: Signal.t
  -> enable: Signal.t
  -> stages: int
  -> Signal.t list
  -> (Signal.t, Signal.t list) result

module For_testing : sig
  (** A combinational implementation for writing proofs. *)
  val create_combinational
    : (module Comb.S with type t = 'a)
    -> stages: int
    -> 'a list
    -> ('a, 'a list) result
end

