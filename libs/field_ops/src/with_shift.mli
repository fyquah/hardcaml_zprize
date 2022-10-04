(** Internal module. *)

open Hardcaml

type t =
  { x : Signal.t
  ; shift : int
  }

val width : t -> int
val no_shift : Signal.t -> t
val create : shift:int -> Signal.t -> t
val sll : t -> by:int -> t
val map : f:(Signal.t -> Signal.t) -> t -> t
val uresize : t -> int -> t

val pipe_add
  :  scope:Scope.t
  -> enable:Signal.t
  -> clock:Signal.t
  -> stages:int
  -> t list
  -> t

val sum : t list -> t
val mixed : init:t -> [ `Add of t | `Sub of t ] list -> t
val to_signal : t -> Signal.t
