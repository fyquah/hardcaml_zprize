open Core
open Hardcaml
open Hardcaml_axi
open Hardcaml_waveterm
open Bits

module Rd = struct
  type 'a t =
    { rd1 : 'a [@bits 32]
    ; rd2 : 'a [@bits 16]
    }
  [@@deriving sexp_of, hardcaml]
end

module Wr = struct
  type 'a t =
    { wr1 : 'a [@bits 32]
    ; wr2 : 'a [@bits 16]
    }
  [@@deriving sexp_of, hardcaml]
end

module Bank = Axi32.Lite.Register_bank (Rd) (Wr)
module Sim = Cyclesim.With_interface (Bank.I) (Bank.O)

let create_sim () =
  let scope = Scope.create ~flatten_design:true () in
  Waveform.create (Sim.create (Bank.create scope))
;;

let ( <--. ) dst src = dst := Bits.of_int ~width:(Bits.width !dst) src

let write_wdata (sim : Sim.t) wdata =
  let inputs = Cyclesim.inputs sim in
  let outputs_before = Cyclesim.outputs ~clock_edge:Before sim in
  inputs.master_to_slave.wvalid := vdd;
  inputs.master_to_slave.wdata <--. wdata;
  Cyclesim.cycle sim;
  while not (Bits.is_vdd !(outputs_before.slave_to_master.wready)) do
    Cyclesim.cycle sim
  done;
  inputs.master_to_slave.wvalid := gnd;
  inputs.master_to_slave.wdata <--. 0
;;

let write_awaddr (sim : Sim.t) awaddr =
  let inputs = Cyclesim.inputs sim in
  let outputs_before = Cyclesim.outputs ~clock_edge:Before sim in
  inputs.master_to_slave.awaddr <--. awaddr;
  inputs.master_to_slave.awvalid <--. 1;
  Cyclesim.cycle sim;
  while not (Bits.is_vdd !(outputs_before.slave_to_master.awready)) do
    Cyclesim.cycle sim
  done;
  inputs.master_to_slave.awaddr <--. 0;
  inputs.master_to_slave.awvalid <--. 0
;;

let write_araddr (sim : Sim.t) araddr =
  let inputs = Cyclesim.inputs sim in
  let outputs_before = Cyclesim.outputs ~clock_edge:Before sim in
  inputs.master_to_slave.araddr <--. araddr;
  inputs.master_to_slave.arvalid <--. 1;
  Cyclesim.cycle sim;
  while not (Bits.is_vdd !(outputs_before.slave_to_master.arready)) do
    Cyclesim.cycle sim
  done;
  inputs.master_to_slave.araddr <--. 0;
  inputs.master_to_slave.arvalid <--. 0
;;

let accept_write_response (sim : Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs_before = Cyclesim.outputs ~clock_edge:Before sim in
  inputs.master_to_slave.bready := vdd;
  Cyclesim.cycle sim;
  while is_vdd !(outputs_before.slave_to_master.bvalid) do
    Cyclesim.cycle sim
  done;
  inputs.master_to_slave.bready := gnd
;;

let accept_read_response (sim : Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs_before = Cyclesim.outputs ~clock_edge:Before sim in
  inputs.master_to_slave.rready := vdd;
  Cyclesim.cycle sim;
  while is_vdd !(outputs_before.slave_to_master.rvalid) do
    Cyclesim.cycle sim
  done;
  inputs.master_to_slave.rready := gnd;
  Bits.to_int !(outputs_before.slave_to_master.rdata)
;;

let%expect_test "Back to back register writes without having to wait for data" =
  let waves, sim = create_sim () in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let print_write_values () =
    let write_values = Wr.map ~f:(fun x -> Bits.to_int !x) outputs.write_values in
    Stdio.print_s [%message (write_values : Int.Hex.t Wr.t)]
  in
  inputs.clear := vdd;
  Cyclesim.cycle sim;
  inputs.clear := gnd;
  write_awaddr sim 4;
  write_wdata sim 0xdeadbeef;
  accept_write_response sim;
  Cyclesim.cycle sim;
  print_write_values ();
  [%expect {| (write_values ((wr1 0x0) (wr2 0xbeef))) |}];
  write_awaddr sim 0;
  write_wdata sim 0xaaaa;
  accept_write_response sim;
  print_write_values ();
  [%expect {| (write_values ((wr1 0xaaaa) (wr2 0xbeef))) |}];
  Waveform.print ~display_height:60 waves ~wave_width:2;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──│
    │               ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  │
    │clear          ││──────┐                                            │
    │               ││      └────────────────────────────────────────────│
    │               ││───────────────────────────────────────────────────│
    │araddr         ││ 00000000                                          │
    │               ││───────────────────────────────────────────────────│
    │arvalid        ││                                                   │
    │               ││───────────────────────────────────────────────────│
    │               ││──────┬───────────┬────────────────────────────────│
    │awaddr         ││ 0000.│00000004   │00000000                        │
    │               ││──────┴───────────┴────────────────────────────────│
    │awvalid        ││      ┌───────────┐                       ┌────────│
    │               ││──────┘           └───────────────────────┘        │
    │bready         ││                        ┌───────────┐              │
    │               ││────────────────────────┘           └──────────────│
    │               ││───────────────────────────────────────────────────│
    │rd1            ││ 00000000                                          │
    │               ││───────────────────────────────────────────────────│
    │               ││───────────────────────────────────────────────────│
    │rd2            ││ 0000                                              │
    │               ││───────────────────────────────────────────────────│
    │rready         ││                                                   │
    │               ││───────────────────────────────────────────────────│
    │               ││──────────────────┬─────┬──────────────────────────│
    │wdata          ││ 00000000         │DEAD.│00000000                  │
    │               ││──────────────────┴─────┴──────────────────────────│
    │wvalid         ││                  ┌─────┐                          │
    │               ││──────────────────┘     └──────────────────────────│
    │arready        ││                                                   │
    │               ││───────────────────────────────────────────────────│
    │awready        ││            ┌─────┐                             ┌──│
    │               ││────────────┘     └─────────────────────────────┘  │
    │               ││───────────────────────────────────────────────────│
    │bresp          ││ 0                                                 │
    │               ││───────────────────────────────────────────────────│
    │bvalid         ││                        ┌─────┐                    │
    │               ││────────────────────────┘     └────────────────────│
    │               ││───────────────────────────────────────────────────│
    │rdata          ││ 00000000                                          │
    │               ││───────────────────────────────────────────────────│
    │               ││───────────────────────────────────────────────────│
    │rresp          ││ 0                                                 │
    │               ││───────────────────────────────────────────────────│
    │rvalid         ││                                                   │
    │               ││───────────────────────────────────────────────────│
    │               ││───────────────────────────────────────────────────│
    │wr1            ││ 00000000                                          │
    │               ││───────────────────────────────────────────────────│
    │               ││──────────────────────────────┬────────────────────│
    │wr2            ││ 0000                         │BEEF                │
    │               ││──────────────────────────────┴────────────────────│
    │wready         ││                  ┌─────┐                          │
    │               ││──────────────────┘     └──────────────────────────│
    │               ││                                                   │
    │               ││                                                   │
    │               ││                                                   │
    │               ││                                                   │
    └───────────────┘└───────────────────────────────────────────────────┘ |}]
;;

(* Test waiting for awhile for wdata to arrive after awaddr handshake.  *)
let%expect_test "Register write wait for data" =
  let waves, sim = create_sim () in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let print_write_values () =
    let write_values = Wr.map ~f:(fun x -> Bits.to_int !x) outputs.write_values in
    Stdio.print_s [%message (write_values : Int.Hex.t Wr.t)]
  in
  inputs.clear := vdd;
  Cyclesim.cycle sim;
  inputs.clear := gnd;
  write_awaddr sim 4;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  write_wdata sim 0xdeadbeef;
  accept_write_response sim;
  print_write_values ();
  [%expect {| (write_values ((wr1 0x0) (wr2 0xbeef))) |}];
  Waveform.print ~display_height:60 waves ~wave_width:2;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──│
    │               ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  │
    │clear          ││──────┐                                            │
    │               ││      └────────────────────────────────────────────│
    │               ││───────────────────────────────────────────────────│
    │araddr         ││ 00000000                                          │
    │               ││───────────────────────────────────────────────────│
    │arvalid        ││                                                   │
    │               ││───────────────────────────────────────────────────│
    │               ││──────┬───────────┬────────────────────────────────│
    │awaddr         ││ 0000.│00000004   │00000000                        │
    │               ││──────┴───────────┴────────────────────────────────│
    │awvalid        ││      ┌───────────┐                                │
    │               ││──────┘           └────────────────────────────────│
    │bready         ││                                                ┌──│
    │               ││────────────────────────────────────────────────┘  │
    │               ││───────────────────────────────────────────────────│
    │rd1            ││ 00000000                                          │
    │               ││───────────────────────────────────────────────────│
    │               ││───────────────────────────────────────────────────│
    │rd2            ││ 0000                                              │
    │               ││───────────────────────────────────────────────────│
    │rready         ││                                                   │
    │               ││───────────────────────────────────────────────────│
    │               ││──────────────────────────────────────────┬─────┬──│
    │wdata          ││ 00000000                                 │DEAD.│00│
    │               ││──────────────────────────────────────────┴─────┴──│
    │wvalid         ││                                          ┌─────┐  │
    │               ││──────────────────────────────────────────┘     └──│
    │arready        ││                                                   │
    │               ││───────────────────────────────────────────────────│
    │awready        ││            ┌─────┐                                │
    │               ││────────────┘     └────────────────────────────────│
    │               ││───────────────────────────────────────────────────│
    │bresp          ││ 0                                                 │
    │               ││───────────────────────────────────────────────────│
    │bvalid         ││                                                ┌──│
    │               ││────────────────────────────────────────────────┘  │
    │               ││───────────────────────────────────────────────────│
    │rdata          ││ 00000000                                          │
    │               ││───────────────────────────────────────────────────│
    │               ││───────────────────────────────────────────────────│
    │rresp          ││ 0                                                 │
    │               ││───────────────────────────────────────────────────│
    │rvalid         ││                                                   │
    │               ││───────────────────────────────────────────────────│
    │               ││───────────────────────────────────────────────────│
    │wr1            ││ 00000000                                          │
    │               ││───────────────────────────────────────────────────│
    │               ││───────────────────────────────────────────────────│
    │wr2            ││ 0000                                              │
    │               ││───────────────────────────────────────────────────│
    │wready         ││                  ┌─────────────────────────────┐  │
    │               ││──────────────────┘                             └──│
    │               ││                                                   │
    │               ││                                                   │
    │               ││                                                   │
    │               ││                                                   │
    └───────────────┘└───────────────────────────────────────────────────┘ |}]
;;

let%expect_test "Back-to-back reads" =
  let waves, sim = create_sim () in
  let inputs = Cyclesim.inputs sim in
  inputs.clear := vdd;
  Cyclesim.cycle sim;
  inputs.clear := gnd;
  inputs.read_values.rd1 <--. 0xaaaa;
  inputs.read_values.rd2 <--. 0xbbbb;
  write_araddr sim 4;
  print_s (Int.Hex.sexp_of_t (accept_read_response sim));
  [%expect {| 0xbbbb |}];
  (* Read word 0. Delay a few cycles beefore accepting the response *)
  write_araddr sim 0;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  print_s (Int.Hex.sexp_of_t (accept_read_response sim));
  [%expect {| 0xaaaa |}];
  Waveform.print ~display_height:60 waves ~wave_width:1;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐│
    │               ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └│
    │clear          ││────┐                                              │
    │               ││    └───────────────────────────────────────       │
    │               ││────┬───────┬───────────────────────────────       │
    │araddr         ││ 00.│000000.│00000000                              │
    │               ││────┴───────┴───────────────────────────────       │
    │arvalid        ││    ┌───────┐       ┌───────┐                      │
    │               ││────┘       └───────┘       └───────────────       │
    │               ││────────────────────────────────────────────       │
    │awaddr         ││ 00000000                                          │
    │               ││────────────────────────────────────────────       │
    │awvalid        ││                                                   │
    │               ││────────────────────────────────────────────       │
    │bready         ││                                                   │
    │               ││────────────────────────────────────────────       │
    │               ││────┬───────────────────────────────────────       │
    │rd1            ││ 00.│0000AAAA                                      │
    │               ││────┴───────────────────────────────────────       │
    │               ││────┬───────────────────────────────────────       │
    │rd2            ││ 00.│BBBB                                          │
    │               ││────┴───────────────────────────────────────       │
    │rready         ││            ┌───────┐               ┌───────       │
    │               ││────────────┘       └───────────────┘              │
    │               ││────────────────────────────────────────────       │
    │wdata          ││ 00000000                                          │
    │               ││────────────────────────────────────────────       │
    │wvalid         ││                                                   │
    │               ││────────────────────────────────────────────       │
    │arready        ││        ┌───┐           ┌───┐                      │
    │               ││────────┘   └───────────┘   └───────────────       │
    │awready        ││                                                   │
    │               ││────────────────────────────────────────────       │
    │               ││────────────────────────────────────────────       │
    │bresp          ││ 0                                                 │
    │               ││────────────────────────────────────────────       │
    │bvalid         ││                                                   │
    │               ││────────────────────────────────────────────       │
    │               ││────────────┬───────────────┬───────────────       │
    │rdata          ││ 00000000   │0000BBBB       │0000AAAA              │
    │               ││────────────┴───────────────┴───────────────       │
    │               ││────────────────────────────────────────────       │
    │rresp          ││ 0                                                 │
    │               ││────────────────────────────────────────────       │
    │rvalid         ││            ┌───┐           ┌───────────┐          │
    │               ││────────────┘   └───────────┘           └───       │
    │               ││────────────────────────────────────────────       │
    │wr1            ││ 00000000                                          │
    │               ││────────────────────────────────────────────       │
    │               ││────────────────────────────────────────────       │
    │wr2            ││ 0000                                              │
    │               ││────────────────────────────────────────────       │
    │wready         ││                                                   │
    │               ││────────────────────────────────────────────       │
    │               ││                                                   │
    │               ││                                                   │
    │               ││                                                   │
    │               ││                                                   │
    └───────────────┘└───────────────────────────────────────────────────┘ |}]
;;
