open! Base
open Hardcaml

let latency = 6

let create ~clock a b =
  let pipe x =
    Gf.Signal.to_bits x
    |> Signal.pipeline (Reg_spec.create ~clock ()) ~n:1
    |> Gf.Signal.of_bits
  in
  Gf.Signal.mul ~pipe (pipe (Gf.Signal.of_bits a)) (pipe (Gf.Signal.of_bits b))
  |> Gf.Signal.to_bits
;;
