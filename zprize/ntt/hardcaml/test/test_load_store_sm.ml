open! Core
open Hardcaml
open Hardcaml_waveterm

module Store_sm = Zprize_ntt.Store_sm.Make (struct
  let logn = 4
  let support_4step_twiddle = false
  let logcores = 0
  let logblocks = 0
  let memory_layout = Zprize_ntt.Memory_layout.Optimised_layout_single_port
end)

module Sim = Cyclesim.With_interface (Store_sm.I) (Store_sm.O)

let test_store_sm () =
  let sim = Sim.create (Store_sm.create (Scope.create ())) in
  let i = Cyclesim.inputs sim in
  let waves, sim = Waveform.create sim in
  let cycle () = Cyclesim.cycle sim in
  i.clear := Bits.vdd;
  cycle ();
  i.clear := Bits.gnd;
  i.start := Bits.vdd;
  cycle ();
  i.start := Bits.gnd;
  i.tready := Bits.vdd;
  cycle ();
  i.tready := Bits.gnd;
  cycle ();
  cycle ();
  i.tready := Bits.vdd;
  cycle ();
  cycle ();
  i.tready := Bits.gnd;
  cycle ();
  i.tready := Bits.vdd;
  cycle ();
  i.tready := Bits.gnd;
  cycle ();
  cycle ();
  for _ = 0 to 10 do
    cycle ()
  done;
  waves
;;

let%expect_test "store sm" =
  let waves = test_store_sm () in
  Waveform.print ~display_width:90 ~display_height:25 ~wave_width:1 waves;
  [%expect {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
    │clock             ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ │
    │                  ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─│
    │clear             ││────┐                                                               │
    │                  ││    └───────────────────────────────────────────────────────────────│
    │first_4step_pass  ││                                                                    │
    │                  ││────────────────────────────────────────────────────────────────────│
    │start             ││    ┌───┐                                                           │
    │                  ││────┘   └───────────────────────────────────────────────────────────│
    │tready            ││        ┌───┐       ┌───────┐   ┌───┐                               │
    │                  ││────────┘   └───────┘       └───┘   └───────────────────────────────│
    │block             ││                                                                    │
    │                  ││────────────────────────────────────────────────────────────────────│
    │done_             ││────────┐                                                           │
    │                  ││        └───────────────────────────────────────────────────────────│
    │                  ││────────────┬───┬───┬───┬───┬───┬───┬───┬───┬───────────────────────│
    │rd_addr           ││ 0          │1  │2  │3  │4  │5  │6  │7  │8  │9                      │
    │                  ││────────────┴───┴───┴───┴───┴───┴───┴───┴───┴───────────────────────│
    │rd_any            ││        ┌───────────────────────────────────┐                       │
    │                  ││────────┘                                   └───────────────────────│
    │rd_en             ││        ┌───────────────────────────────────┐                       │
    │                  ││────────┘                                   └───────────────────────│
    │tvalid            ││                                            ┌───────────────────────│
    │                  ││────────────────────────────────────────────┘                       │
    └──────────────────┘└────────────────────────────────────────────────────────────────────┘ |}]
;;
