(* Test the block transposer component. *)

open Core
open Hardcaml
module Waveform = Hardcaml_waveterm.Waveform
module Transposer = Hardcaml_ntt.Transposer
module Sim = Cyclesim.With_interface (Transposer.I) (Transposer.O)

let () = Hardcaml.Caller_id.set_mode Full_trace
let transposer_height = Transposer.transposer_height

let rectangle_size ~transposer_depth_in_cycles =
  let number_of_elements_in_row = transposer_depth_in_cycles * 8 in
  transposer_height * number_of_elements_in_row
;;

let pump_data ~transposer_depth_in_cycles ~iter (sim : Sim.t) =
  let number_of_elements_in_row = transposer_depth_in_cycles * 8 in
  let inputs = Cyclesim.inputs sim in
  let outputs_before = Cyclesim.outputs ~clock_edge:Before sim in
  inputs.transposer_in.tvalid := Bits.vdd;
  for i = 0 to 7 do
    for j = 0 to transposer_depth_in_cycles - 1 do
      let tdata =
        List.init 8 ~f:(fun k ->
          (iter * rectangle_size ~transposer_depth_in_cycles)
          + (i * number_of_elements_in_row)
          + (j * 8)
          + k)
      in
      inputs.transposer_in.tdata
        := Bits.concat_lsb (List.map ~f:(Bits.of_int ~width:64) tdata);
      while
        Cyclesim.cycle sim;
        Bits.is_gnd !(outputs_before.transposer_in_dest.tready)
      do
        ()
      done
    done
  done;
  inputs.transposer_in.tvalid := Bits.gnd
;;

let receive_and_check_data ~iter ~transposer_depth_in_cycles (sim : Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs_before = Cyclesim.outputs ~clock_edge:Before sim in
  let rectangle_size = rectangle_size ~transposer_depth_in_cycles in
  inputs.transposer_out_dest.tready := Bits.vdd;
  for i = 0 to (transposer_depth_in_cycles * 8) - 1 do
    while
      Cyclesim.cycle sim;
      Bits.is_gnd !(outputs_before.transposer_out.tvalid)
    do
      ()
    done;
    !(outputs_before.transposer_out.tdata)
    |> Bits.split_lsb ~part_width:64
    |> List.iteri ~f:(fun j x ->
         let obtained = Bits.to_int x in
         let expected =
           (iter * rectangle_size) + (j * transposer_depth_in_cycles * 8) + i
         in
         if obtained <> expected
         then (
           Cyclesim.cycle sim;
           raise_s
             [%message
               "Check failed!" (i : int) (j : int) (expected : int) (obtained : int)]))
  done;
  inputs.transposer_out_dest.tready := Bits.gnd;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim
;;

let display_rules =
  Hardcaml_waveterm.Display_rule.
    [ port_name_is "clock" ~wave_format:Bit
    ; port_name_is "in_tvalid" ~wave_format:Bit
    ; port_name_is "in_tready" ~wave_format:Bit
    ; port_name_is "out_tvalid" ~wave_format:Bit
    ; port_name_is "out_tready" ~wave_format:Bit
    ; port_name_is "wr_pos" ~wave_format:Bit
    ; port_name_is "rd_pos" ~wave_format:Bit
    ]
;;

let create_sim ~transposer_depth_in_cycles =
  let scope = Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true () in
  let config = Cyclesim.Config.trace_all in
  Waveform.create
    (Sim.create ~config (Transposer.create ~transposer_depth_in_cycles scope))
;;

let test_back_to_back_read_write
  ?waveform_name
  ?(num_iters = 5)
  ~transposer_depth_in_cycles
  ()
  =
  let waves, sim = create_sim ~transposer_depth_in_cycles in
  try
    for iter = 0 to num_iters - 1 do
      pump_data ~transposer_depth_in_cycles ~iter sim;
      receive_and_check_data ~iter ~transposer_depth_in_cycles sim
    done
  with
  | exn ->
    Option.iter waveform_name ~f:(fun waveform_name ->
      let fname = Printf.sprintf "%s.hardcamlwaveform" waveform_name in
      Waveform.Serialize.marshall waves fname;
      Stdio.printf "Test failed -- saving waveform to %s" fname);
    Exn.reraise exn "test failed"
;;

let test_2_read_after_2_writes
  ?waveform_name
  ?(wave_width = 0)
  ~transposer_depth_in_cycles
  ()
  =
  let waves, sim = create_sim ~transposer_depth_in_cycles in
  try
    let outputs = Cyclesim.outputs sim in
    pump_data ~transposer_depth_in_cycles ~iter:0 sim;
    pump_data ~transposer_depth_in_cycles ~iter:1 sim;
    (* Verify that the core is not ready to accept more data. *)
    for _ = 1 to 3 do
      Cyclesim.cycle sim;
      assert (Bits.is_gnd !(outputs.transposer_in_dest.tready))
    done;
    (* Now, peek at the data and make sure it is correct. *)
    receive_and_check_data ~iter:0 ~transposer_depth_in_cycles sim;
    receive_and_check_data ~iter:1 ~transposer_depth_in_cycles sim;
    Waveform.print ~display_rules ~display_width:105 ~display_height:30 ~wave_width waves
  with
  | exn ->
    Option.iter waveform_name ~f:(fun waveform_name ->
      let fname = Printf.sprintf "%s.hardcamlwaveform" waveform_name in
      Waveform.Serialize.marshall waves fname;
      Stdio.printf "Test failed -- saving waveform to %s" fname);
    Exn.reraise exn "test failed"
;;

let%expect_test "Read data after two writes (transposer depth = 1)" =
  test_2_read_after_2_writes ~transposer_depth_in_cycles:1 ();
  [%expect
    {|
    ┌Signals───────────┐┌Waves──────────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌│
    │                  ││ └┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘│
    │in_tvalid         ││────────────────────────────────────┐                                              │
    │                  ││                                    └───────────────────────────────────────────── │
    │in_tready         ││  ┌───────────────┐ ┌───────────────┐                       ┌───────────────────── │
    │                  ││──┘               └─┘               └───────────────────────┘                      │
    │out_tvalid        ││                    ┌─────────────────────────────────────┐ ┌─────────────────┐    │
    │                  ││────────────────────┘                                     └─┘                 └─── │
    │out_tready        ││                                          ┌───────────────┐   ┌───────────────┐    │
    │                  ││──────────────────────────────────────────┘               └───┘               └─── │
    │                  ││──────────────────┬─────────────────┬───────────────────────────────────────────── │
    │wr_pos            ││ 00               │01               │10                                            │
    │                  ││──────────────────┴─────────────────┴───────────────────────────────────────────── │
    │                  ││──────────────────────────────────────────────────────────┬───────────────────┬─── │
    │rd_pos            ││ 00                                                       │01                 │10  │
    │                  ││──────────────────────────────────────────────────────────┴───────────────────┴─── │
    │                  ││                                                                                   │
    │                  ││                                                                                   │
    │                  ││                                                                                   │
    │                  ││                                                                                   │
    │                  ││                                                                                   │
    │                  ││                                                                                   │
    │                  ││                                                                                   │
    │                  ││                                                                                   │
    │                  ││                                                                                   │
    │                  ││                                                                                   │
    │                  ││                                                                                   │
    │                  ││                                                                                   │
    └──────────────────┘└───────────────────────────────────────────────────────────────────────────────────┘ |}]
;;

let%expect_test "Read data after two writes (transposer depth = 2)" =
  test_2_read_after_2_writes
    ~waveform_name:"foo"
    ~transposer_depth_in_cycles:2
    ~wave_width:(-1)
    ();
  [%expect
    {|
    ┌Signals───────────┐┌Waves──────────────────────────────────────────────────────────────────────────────┐
    │clock             ││╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥│
    │                  ││╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨│
    │in_tvalid         ││──────────────────────────────────┐                                                │
    │                  ││                                  └──────────────────────────────────────          │
    │in_tready         ││ ┌───────────────┐┌───────────────┐                   ┌──────────────────          │
    │                  ││─┘               └┘               └───────────────────┘                            │
    │out_tvalid        ││                  ┌──────────────────────────────────┐┌────────────────┐           │
    │                  ││──────────────────┘                                  └┘                └─          │
    │out_tready        ││                                     ┌───────────────┐ ┌───────────────┐           │
    │                  ││─────────────────────────────────────┘               └─┘               └─          │
    │                  ││─────────────────┬────────────────┬──────────────────────────────────────          │
    │wr_pos            ││ 00              │01              │10                                              │
    │                  ││─────────────────┴────────────────┴──────────────────────────────────────          │
    │                  ││─────────────────────────────────────────────────────┬─────────────────┬─          │
    │rd_pos            ││ 00                                                  │01               │.          │
    │                  ││─────────────────────────────────────────────────────┴─────────────────┴─          │
    │                  ││                                                                                   │
    │                  ││                                                                                   │
    │                  ││                                                                                   │
    │                  ││                                                                                   │
    │                  ││                                                                                   │
    │                  ││                                                                                   │
    │                  ││                                                                                   │
    │                  ││                                                                                   │
    │                  ││                                                                                   │
    │                  ││                                                                                   │
    │                  ││                                                                                   │
    │                  ││                                                                                   │
    └──────────────────┘└───────────────────────────────────────────────────────────────────────────────────┘ |}]
;;

let%expect_test "back-to-back read writes (transposer depth = 1)" =
  test_back_to_back_read_write ~transposer_depth_in_cycles:1 ();
  [%expect {| |}]
;;

let%expect_test "back-to-back read writes (transposer depth = 2)" =
  test_back_to_back_read_write ~transposer_depth_in_cycles:2 ();
  [%expect {| |}]
;;

let%expect_test "back-to-back read writes (transposer depth = 4)" =
  test_back_to_back_read_write ~transposer_depth_in_cycles:4 ();
  [%expect {| |}]
;;
