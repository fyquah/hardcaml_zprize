open Core
open Hardcaml
module Waveform = Hardcaml_waveterm.Waveform
module Transposer = Ntts_r_fun.Transposer
module Sim = Cyclesim.With_interface (Transposer.I) (Transposer.O)

let () = Hardcaml.Caller_id.set_mode Full_trace

let pump_data ~iter (sim : Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs_before = Cyclesim.outputs ~clock_edge:Before sim in
  inputs.transposer_in.tvalid := Bits.vdd;
  for i = 0 to 7 do
    inputs.transposer_in.tdata
      := Bits.concat_lsb
           (List.init 32 ~f:(fun x -> Bits.of_int ~width:16 ((iter * 512) + (i * 64) + x)));
    while
      Cyclesim.cycle sim;
      Bits.is_gnd !(outputs_before.transposer_in_dest.tready)
    do
      ()
    done
  done
;;

let%expect_test "" =
  let waves, sim =
    let scope =
      Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ()
    in
    let transposer_depth_in_cycles = 1 in
    Waveform.create (Sim.create (Transposer.create ~transposer_depth_in_cycles scope))
  in
  let outputs = Cyclesim.outputs sim in
  pump_data ~iter:0 sim;
  pump_data ~iter:1 sim;
  (* Verify that the core is not ready to accept more data. *)
  for _ = 1 to 3 do
    Cyclesim.cycle sim;
    assert (Bits.is_gnd !(outputs.transposer_in_dest.tready))
  done;
  Waveform.print ~display_height:30 ~wave_width:1 waves;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐│
    │               ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └│
    │clear          ││                                                   │
    │               ││───────────────────────────────────────────────────│
    │               ││────────┬───┬───┬───┬───┬───┬───┬───┬───────┬───┬──│
    │transposer_in_t││ 001F00.│00.│00.│00.│01.│01.│01.│01.│021F02.│02.│02│
    │               ││────────┴───┴───┴───┴───┴───┴───┴───┴───────┴───┴──│
    │transposer_in_t││───────────────────────────────────────────────────│
    │               ││                                                   │
    │transposer_out_││                                                   │
    │               ││───────────────────────────────────────────────────│
    │transposer_in_t││    ┌───────────────────────────────┐   ┌──────────│
    │               ││────┘                               └───┘          │
    │               ││────────┬───┬───┬───┬───┬───┬───┬───┬──────────────│
    │transposer_out_││ 000000.│00.│00.│00.│00.│00.│00.│00.│01C301C201C101│
    │               ││────────┴───┴───┴───┴───┴───┴───┴───┴──────────────│
    │               ││───────────────────────────────────────────────────│
    │transposer_out_││ FFFFFFFFFFFFFFFF                                  │
    │               ││───────────────────────────────────────────────────│
    │transposer_out_││                                                   │
    │               ││───────────────────────────────────────────────────│
    │               ││───────────────────────────────────────────────────│
    │transposer_out_││ FFFFFFFFFFFFFFFF                                  │
    │               ││───────────────────────────────────────────────────│
    │transposer_out_││                                        ┌──────────│
    │               ││────────────────────────────────────────┘          │
    │               ││                                                   │
    │               ││                                                   │
    └───────────────┘└───────────────────────────────────────────────────┘ |}]
;;
