open! Core
open Hardcaml
open Hardcaml_waveterm

module Ntt_4step = Ntts_r_fun.Ntt_4step.Make (struct
  let logn = 4
  let twiddle_4step_config = None
  let logcores = 3
end)

module Kernel = Ntt_4step.Kernel
module Store_sm = Kernel.Store_sm

let test_store_sm () =
  let start = Signal.input "start_store" 1 in
  let i = Kernel.I.(map t ~f:(fun (n, b) -> Signal.input n b)) in
  let o = Store_sm.create i ~start in
  let o = Store_sm.(map2 port_names o ~f:Signal.output) in
  let circuit = Circuit.create_exn ~name:"store_sm" (Store_sm.to_list o) in
  let sim = Cyclesim.create circuit in
  let i =
    Kernel.I.(
      map t ~f:(fun (n, b) ->
        try Cyclesim.in_port sim n with
        | _ -> ref (Bits.zero b)))
  in
  let start = Cyclesim.in_port sim "start_store" in
  let _o = Store_sm.(map port_names ~f:(Cyclesim.out_port sim)) in
  let waves, sim = Waveform.create sim in
  let cycle () = Cyclesim.cycle sim in
  i.clear := Bits.vdd;
  cycle ();
  i.clear := Bits.gnd;
  start := Bits.vdd;
  cycle ();
  start := Bits.gnd;
  i.data_out_dest.tready := Bits.vdd;
  cycle ();
  i.data_out_dest.tready := Bits.gnd;
  cycle ();
  cycle ();
  i.data_out_dest.tready := Bits.vdd;
  cycle ();
  cycle ();
  i.data_out_dest.tready := Bits.gnd;
  cycle ();
  i.data_out_dest.tready := Bits.vdd;
  cycle ();
  i.data_out_dest.tready := Bits.gnd;
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
    │data_out_dest_trea││        ┌───┐       ┌───────┐   ┌───┐                               │
    │                  ││────────┘   └───────┘       └───┘   └───────────────────────────────│
    │start_store       ││    ┌───┐                                                           │
    │                  ││────┘   └───────────────────────────────────────────────────────────│
    │done_             ││────────┐                                                           │
    │                  ││        └───────────────────────────────────────────────────────────│
    │                  ││────────────┬───────────┬───┬───────┬───────────────────────────────│
    │rd_addr           ││ 0          │1          │2  │3      │4                              │
    │                  ││────────────┴───────────┴───┴───────┴───────────────────────────────│
    │rd_en             ││        ┌───┐       ┌───────┐   ┌───┐                               │
    │                  ││────────┘   └───────┘       └───┘   └───────────────────────────────│
    │tvalid            ││            ┌───────────────────────────────────────────────────────│
    │                  ││────────────┘                                                       │
    │                  ││                                                                    │
    │                  ││                                                                    │
    │                  ││                                                                    │
    │                  ││                                                                    │
    │                  ││                                                                    │
    │                  ││                                                                    │
    └──────────────────┘└────────────────────────────────────────────────────────────────────┘ |}]
;;
