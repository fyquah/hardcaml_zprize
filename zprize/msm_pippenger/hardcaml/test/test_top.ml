open Hardcaml
open Hardcaml_waveterm
open Msm_pippenger

module Small_config = struct
  let field_bits = 377
  let scalar_bits = 253
  let controller_log_stall_fifo_depth = 2
  let window_size_bits = 12
  let ram_read_latency = 1
end

module Top = Top.Make (Small_config)
module I = Top.I
module O = Top.O
module Sim = Cyclesim.With_interface (I) (O)

let create_sim () =
  let scope = Scope.create ~flatten_design:true () in
  Sim.create
    ~config:Cyclesim.Config.trace_all
    (Top.hierarchical ~build_mode:Simulation scope)
;;

let run_small_test () =
  let sim = create_sim () in
  let waves, _sim = Waveform.create sim in
  waves
;;

let%expect_test "Test over small input size" =
  let _waves = run_small_test () in
  ()
;;

let waveform = run_small_test
