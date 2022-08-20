open! Core
open Hardcaml
open Hardcaml_waveterm

module Config = struct
  let window_size_bits = 13
  let num_windows = 7
  let affine_point_bits = 377 * 2
  let pipeline_depth = 150
end

module Controller = Pippenger.Controller.Make (Config)
module Sim = Cyclesim.With_interface (Controller.I) (Controller.O)

let%expect_test "" =
  let sim =
    Sim.create
      ~config:Cyclesim.Config.trace_all
      (Controller.create (Scope.create ~flatten_design:true ()))
  in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  inputs.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clear := Bits.gnd;
  inputs.start := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.start := Bits.gnd;
  inputs.scalar_valid := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.scalar_valid := Bits.gnd;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Waveform.print ~display_width:100 ~display_height:35 ~wave_width:1 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─│
    │                  ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ │
    │clear             ││────┐                                                                         │
    │                  ││    └───────────────────                                                      │
    │scalar_valid      ││        ┌───┐                                                                 │
    │                  ││────────┘   └───────────                                                      │
    │start             ││    ┌───┐                                                                     │
    │                  ││────┘   └───────────────                                                      │
    │                  ││────────────────────────                                                      │
    │adder_affine_point││ 0000000000000000000000.                                                      │
    │                  ││────────────────────────                                                      │
    │bubble            ││                                                                              │
    │                  ││────────────────────────                                                      │
    │                  ││────────────────────────                                                      │
    │bucket            ││ 0000                                                                         │
    │                  ││────────────────────────                                                      │
    │done_             ││────────┐                                                                     │
    │                  ││        └───────────────                                                      │
    │execute           ││                                                                              │
    │                  ││────────────────────────                                                      │
    │scalar_read       ││                                                                              │
    │                  ││────────────────────────                                                      │
    │                  ││────────────────────────                                                      │
    │window            ││ 0                                                                            │
    │                  ││────────────────────────                                                      │
    │                  ││────────┬───────────────                                                      │
    │STATE             ││ 0      │1                                                                    │
    │                  ││────────┴───────────────                                                      │
    │gnd               ││                                                                              │
    │                  ││────────────────────────                                                      │
    │vdd               ││────────────────────────                                                      │
    │                  ││                                                                              │
    │                  ││                                                                              │
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────┘ |}]
;;
