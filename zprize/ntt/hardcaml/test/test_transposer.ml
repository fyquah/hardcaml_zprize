open Core
open Hardcaml
module Transposer = Ntts_r_fun.Transposer
module Sim = Cyclesim.With_interface (Transposer.I) (Transposer.O)

let () = Hardcaml.Caller_id.set_mode Full_trace

let%expect_test "" =
  let _sim =
    let scope =
      Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ()
    in
    let transposer_depth_in_cycles = 1 in
    Sim.create
      ~config:Cyclesim.Config.trace_all
      (Transposer.create ~transposer_depth_in_cycles scope)
  in
  ()
;;
