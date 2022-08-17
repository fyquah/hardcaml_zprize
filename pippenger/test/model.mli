type t [@@deriving sexp_of]

module Summary : sig
  type nonrec t = t [@@deriving sexp_of]
end

val test_uneven : ?verbose:bool -> int -> t
val test_even : ?verbose:bool -> int -> t
