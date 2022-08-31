open Base
include Gf_intf.S

(** {2 Misc} *)

val inverse : t -> t
val pow : t -> int -> t
val pp : Formatter.t -> t -> unit
val random : unit -> t
