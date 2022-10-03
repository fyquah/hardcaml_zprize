(** Twiddle factor update interface. *)

open! Base

val sync_cycles : int
val sync_cycles_width : int

type 'a t =
  { valid : 'a
  ; index : 'a
  }
[@@deriving sexp_of, hardcaml]
