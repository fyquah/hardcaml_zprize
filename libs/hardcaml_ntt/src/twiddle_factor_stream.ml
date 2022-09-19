open Base
open Hardcaml
open Signal
module Var = Always.Variable

let pipe_length = Multiplier.latency + 1

module I = struct
  type 'a t =
    { clock : 'a
    ; start_twiddles : 'a
    ; omegas : 'a list [@bits Gf.Signal.num_bits] [@length pipe_length]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { w : 'a [@bits Gf.Signal.num_bits] } [@@deriving sexp_of, hardcaml]
end

let create _scope (i : _ I.t) =
  let pipe_length = List.length i.omegas in
  let spec = Reg_spec.create ~clock:i.clock () in
  let w = Var.reg ~width:Gf.Signal.num_bits spec in
  let omega_step, omega_pipe = List.last_exn i.omegas, List.drop_last_exn i.omegas in
  let multiplier_output = Multiplier.create ~clock:i.clock w.value omega_step in
  let count = Var.reg spec ~width:(Int.ceil_log2 (Multiplier.latency + 1)) in
  Always.(
    compile
      [ w <-- mux count.value (omega_pipe @ [ multiplier_output ])
      ; when_ (count.value <>:. pipe_length - 1) [ count <-- count.value +:. 1 ]
      ; when_ i.start_twiddles [ w <-- Gf.Signal.to_bits Gf.Signal.one; count <--. 0 ]
      ]);
  { O.w = w.value }
;;

let hierarchy scope =
  let module Hier = Hierarchy.In_scope (I) (O) in
  Hier.hierarchical ~name:"twdl" ~scope create
;;

let initial_pipeline_factors root =
  let inverse_root = Roots.inverse.(root) in
  let rec f acc i =
    if i = pipe_length then [] else acc :: f (Gf.Z.( * ) acc inverse_root) (i + 1)
  in
  f inverse_root 0
  |> List.map ~f:(fun x -> Gf.Signal.to_bits (Gf.Signal.of_z (Gf.Z.to_z x)))
;;
