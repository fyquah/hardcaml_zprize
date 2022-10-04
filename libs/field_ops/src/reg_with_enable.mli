(** Shadow Signal.reg and Signal.pipeline because we want ~enable to be a
    non-optional argument.
*)

open Hardcaml
open Signal

val reg : Reg_spec.t -> enable:t -> t -> t
val pipeline : Reg_spec.t -> enable:t -> n:int -> t -> t
