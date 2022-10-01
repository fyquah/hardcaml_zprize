open! Core
open Hardcaml
open Hardcaml_waveterm

module Store_sm = Zprize_ntt.Store_sm.Make (struct
  let logn = 4
  let support_4step_twiddle = false
  let logcores = 0
  let logblocks = 0
  let memory_layout = Zprize_ntt.Memory_layout.Optimised_layout_single_port
  let log_num_streams = 0
end)

let test_store_sm () =
  let module Sim = Cyclesim.With_interface (Store_sm.I) (Store_sm.O) in
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
  [%expect
    {|
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
    │rd_en             ││        ┌───────────────────────────────────┐                       │
    │                  ││────────┘                                   └───────────────────────│
    │tvalid            ││                                            ┌───────────────────────│
    │                  ││────────────────────────────────────────────┘                       │
    │                  ││                                                                    │
    │                  ││                                                                    │
    └──────────────────┘└────────────────────────────────────────────────────────────────────┘ |}]
;;

module Multi_load_sm = Zprize_ntt.Multi_load_sm.Make (struct
  let logn = 3
  let support_4step_twiddle = false
  let logcores = 0
  let logblocks = 0
  let memory_layout = Zprize_ntt.Memory_layout.Normal_layout_multi_port
  let log_num_streams = 1
end)

let ( <-- ) a b = a := Bits.of_int ~width:(Bits.width !a) b

let test_multi_load_sm () =
  let module Sim = Cyclesim.With_interface (Multi_load_sm.I) (Multi_load_sm.O) in
  let sim =
    Sim.create ~config:Cyclesim.Config.trace_all (Multi_load_sm.create (Scope.create ()))
  in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  inputs.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clear := Bits.gnd;
  inputs.start := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.start := Bits.gnd;
  Cyclesim.cycle sim;
  let step x =
    inputs.tvalid <-- x;
    Cyclesim.cycle sim
  in
  step 1;
  step 0;
  step 2;
  step 1;
  step 0;
  step 1;
  step 2;
  step 2;
  step 0;
  step 2;
  (* This is for the next block and ignored *)
  step 2;
  step 1;
  for _ = 0 to 10 do
    Cyclesim.cycle sim
  done;
  waves
;;

let%expect_test "store sm" =
  let waves = test_multi_load_sm () in
  Waveform.print ~display_width:90 ~display_height:25 ~wave_width:1 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
    │clock             ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ │
    │                  ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─│
    │clear             ││────┐                                                               │
    │                  ││    └───────────────────────────────────────────────────────────────│
    │start             ││    ┌───┐                                                           │
    │                  ││────┘   └───────────────────────────────────────────────────────────│
    │                  ││────────────┬───┬───┬───┬───┬───┬───┬───────┬───┬───────┬───────────│
    │tvalid            ││ 0          │1  │0  │2  │1  │0  │1  │2      │0  │2      │1          │
    │                  ││────────────┴───┴───┴───┴───┴───┴───┴───────┴───┴───────┴───────────│
    │done_             ││────────┐                                                           │
    │                  ││        └───────────────────────────────────────────────────────────│
    │select            ││                    ┌───┐           ┌───────┐   ┌───┐               │
    │                  ││────────────────────┘   └───────────┘       └───┘   └───────────────│
    │                  ││────────────┬───┬───┬───┬───┬───┬───┬───────┬───┬───┬───┬───┬───────│
    │tready            ││ 0          │1  │0  │2  │1  │0  │1  │2      │0  │2  │0  │1  │0      │
    │                  ││────────────┴───┴───┴───┴───┴───┴───┴───────┴───┴───┴───┴───┴───────│
    │                  ││────────────────┬───┬───┬───┬───────┬───┬───┬───┬───┬───────┬───────│
    │wr_addr           ││ 0              │1  │4  │1  │2      │5  │6  │3  │7  │3      │0      │
    │                  ││────────────────┴───┴───┴───┴───────┴───┴───┴───┴───┴───────┴───────│
    │wr_en             ││            ┌───┐   ┌───────┐   ┌───────────┐   ┌───┐   ┌───┐       │
    │                  ││────────────┘   └───┘       └───┘           └───┘   └───┘   └───────│
    │                  ││────────┬───────────────────────────────────────────┬───────┬───────│
    │active            ││ 0      │3                                          │1      │0      │
    └──────────────────┘└────────────────────────────────────────────────────────────────────┘ |}]
;;
