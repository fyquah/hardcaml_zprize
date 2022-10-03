(** A global constant to specify the intended level of pipelining in designs. *)

type t =
  | Heavy_pipelining
  | Medium_pipelining
[@@deriving enumerate]

(* NOTE: Heavy_pipelining was empirically found to be better. *)
let t = Heavy_pipelining
