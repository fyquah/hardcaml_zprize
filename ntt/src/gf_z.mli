open Base

type t [@@deriving sexp_of]

(** Solinas prime [2^64 - 2^31 + 1] *)
val modulus : t

val of_z : Z.t -> t
val to_z : t -> Z.t
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val ( * ) : t -> t -> t
val inverse : t -> t
val pow : t -> int -> t
val pp : Formatter.t -> t -> unit
