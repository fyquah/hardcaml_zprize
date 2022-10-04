(** Multistage fully-pipelined ripple-carry-adder (or subtractor).

    This module computs [a OP b OP c OP d...] in fully-piipelined using
    the ripple-carry-add algorithm. Each of the OP are either (+) or (-)
    (they don't have to be the same)

    The fully-pipelined adder breaks the adder down to [stages] chunks, 
    each to be added separately every clock cycles. Registers are placed
    at the inputs and outputs to align them to the same clock cycles.

    If any but the first of the operands are constants, the module will
    generate a specialized module that uses the constant directly throughout
    the adder/subtractor, rather than pipelining it.
*)

open Hardcaml

val latency : stages:int -> int

module Term_and_op : sig
  type 'a t =
    | Add of 'a
    | Sub of 'a
end

(** Output of the adder/subtractor with carry/borrow bits.
 
    When instantiating a pure adder, the [carry] will be the carry bits with
    width = [Int.ceil_log2 num_operands]. For a subtractor, it will the
    [borrow] bits. For a mixed adder/subtractor implementation, you can treat
    [carry @: result] as your signed final result.

    The width of result matches the input operands, the width of carry is
    dependent on the exact operands and number of operands.
*)
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

(** Similar to [mixed], but only with additions. *)
val add
  :  ?name:string
  -> ?instance:string
  -> stages:int
  -> scope:Scope.t
  -> enable:Signal.t
  -> clock:Signal.t
  -> Signal.t list
  -> Signal.t O.t

(** Similar to [mixed], but only with subtractions. *)
val sub
  :  ?name:string
  -> ?instance:string
  -> stages:int
  -> scope:Scope.t
  -> enable:Signal.t
  -> clock:Signal.t
  -> Signal.t list
  -> Signal.t O.t

(** {1 Functions without Carry/Borrow} *)

(** These are similar to the [mixed], [add] and [sub] functions, but without
    the carry/borrow bits.

    Note that [stages = 0] or [stages = 1] is allowed. It will result in a
    non-hierarchical combinational adder and a single-stage adder respectively.
    For adding values of [stages], this module will instantiate a
    a hierarchical module. The rationale for this is we empirically observed
    better synthesis results when we don't instantiate it hierarchically.
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
