type t =
  | Normal_layout_single_port
  | Optimised_layout_single_port
  | Normal_layout_multi_port
[@@deriving sexp_of]

val arg : t Core.Command.Arg_type.t
