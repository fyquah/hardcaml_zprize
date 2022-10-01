open Core

type t =
  | Normal_layout_single_port
  | Optimised_layout_single_port
  | Normal_layout_multi_port
[@@deriving sexp_of]

let arg =
  Command.Arg_type.create (function
    | "normal" -> Normal_layout_single_port
    | "optimised" -> Optimised_layout_single_port
    | _ -> raise_s [%message "Invalid memory layout"])
;;
