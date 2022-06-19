open Hardcaml

type t =
  | Multiply of (Signal.t * Signal.t)
  | Square of Signal.t
