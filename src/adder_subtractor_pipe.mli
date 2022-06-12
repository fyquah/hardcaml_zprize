(** Multistage pipelined ripple-carry-adder.
 *
 * The generated architecture for a single pipeline stage for summing 3 numbers
 * looks something like the following:
 *
 * > LUT > CARRY8 > LUT > CARRY8
 *           ^              ^
 * > LUT > CARRY8 > LUT > CARRY8
 *           ^              ^
 * > LUT > CARRY8 > LUT > CARRY8
 *           ^              ^
 * > LUT > CARRY8 > LUT > CARRY8
 *
 * This architecture have a different CARRY8 output for every add operation.
 * This ensures that the carry-chain have a carry-in input can be used
 * appropriately across multiple pipeline stages
*)

open Hardcaml

type ('result, 'carries) result =
  { result  : 'result
  ; carries : 'carries
  }

val hierarchical
  : scope: Scope.t
  -> clock: Signal.t
  -> enable: Signal.t
  -> op: [ `Add | `Sub ]
  -> stages: int
  -> Signal.t list
  -> (Signal.t, Signal.t list) result

module For_testing : sig
  (** A combinational implementation for writing proofs. *)
  val create_combinational
    : (module Comb.S with type t = 'a)
    -> op: [ `Add | `Sub ]
    -> stages: int
    -> 'a list
    -> ('a, 'a list) result
end
