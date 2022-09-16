open Base
include Gf_intf.S

module Hex : sig
  type nonrec t = t [@@deriving sexp_of]
end

(** {2 Misc} *)

val inverse : t -> t
val pow : t -> int -> t
val pp : Formatter.t -> t -> unit
val random : unit -> t
