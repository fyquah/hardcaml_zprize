(** Multistage pipelined ripple-carry-adder (or subtractor).
 *
 * This module computs [a `op1` b `op2` c `op3` d...], where each of the ops
 * are either (+) or (-) (they don't have to be the same). The output of the
 * module will contain all the results and carries of the following:
 *
 * - a `op1` b
 * - a `op1` b `op2` c
 * - a `op1` b `op2` c `op3` d
 * - ...
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

module Term_and_op : sig
  type 'a t =
    | Add of 'a
    | Sub of 'a
end

(** Output from performing a single [a `op1` b] step. *)
module O : sig
  type 'a t =
    { result : 'a
    ; carry : 'a
    }
  [@@deriving fields]
end

(** Instantiate a hierarchical pipelined adder/subtractor chain. *)
val mixed
  :  ?name:string
  -> ?instance:string
  -> stages:int
  -> scope:Scope.t
  -> enable:Signal.t
  -> clock:Signal.t
  -> init:Signal.t
  -> Signal.t Term_and_op.t list
  -> Signal.t O.t

val add
  :  ?name:string
  -> ?instance:string
  -> stages:int
  -> scope:Scope.t
  -> enable:Signal.t
  -> clock:Signal.t
  -> Signal.t list
  -> Signal.t O.t

(** [sub ~stages ... xs] computes [xs[0] - xs[1] - xs[2] ...] *)
val sub
  :  ?name:string
  -> ?instance:string
  -> stages:int
  -> scope:Scope.t
  -> enable:Signal.t
  -> clock:Signal.t
  -> Signal.t list
  -> Signal.t O.t

(* {2 no_carry functions}.
 *
 * These are similar to the [mixed], [add] and [sub] functions, except they
 * don't return the carry. For [stages] > 1, these essentially calls the
 * version with carries and omits it. For [stages] = 1, this returns the
 * actual operation without instantiation (for the synthesizers to spot
 * patterns more easily)
 *)

val mixed_no_carry
  :  ?name:string
  -> ?instance:string
  -> stages:int
  -> scope:Scope.t
  -> enable:Signal.t
  -> clock:Signal.t
  -> init:Signal.t
  -> Signal.t Term_and_op.t list
  -> Signal.t

val add_no_carry
  :  ?name:string
  -> ?instance:string
  -> stages:int
  -> scope:Scope.t
  -> enable:Signal.t
  -> clock:Signal.t
  -> Signal.t list
  -> Signal.t

val sub_no_carry
  :  ?name:string
  -> ?instance:string
  -> stages:int
  -> scope:Scope.t
  -> enable:Signal.t
  -> clock:Signal.t
  -> Signal.t list
  -> Signal.t
