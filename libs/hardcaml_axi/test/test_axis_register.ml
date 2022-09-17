open Core
open Hardcaml
open Import
module Waveform = Hardcaml_waveterm.Waveform
module Stream = Hardcaml_axi.Axi16.Stream
module Sim = Cyclesim.With_interface (Stream.Register.I) (Stream.Register.O)

let create_sim () =
  let scope = Scope.create ~flatten_design:true () in
  Sim.create (Stream.Register.create scope)
;;

let debug = false

let test ~num_cycles ~probability_up_tvalid ~probability_dn_tready =
  let waves, sim = Waveform.create (create_sim ()) in
  let inputs = Cyclesim.inputs sim in
  let outputs_before = Cyclesim.outputs ~clock_edge:Before sim in
  inputs.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clear := Bits.gnd;
  let accumulator = Accumulator.create () in
  let cycle () =
    let tready = random_bool ~p_true:probability_dn_tready in
    inputs.dn_dest.tready := Bits.of_int ~width:1 (Bool.to_int tready);
    Cyclesim.cycle sim;
    let tvalid = Bits.to_bool !(outputs_before.dn.tvalid) in
    if tready && tvalid
    then (
      let tdata = Bits.to_int !(outputs_before.dn.tdata) in
      Accumulator.push accumulator tdata)
  in
  let input_data = List.init num_cycles ~f:(fun _ -> Random.int (1 lsl 16)) in
  List.iter input_data ~f:(fun x ->
    (* Cycle for awhile, just because *)
    inputs.up.tvalid := Bits.gnd;
    while not (random_bool ~p_true:probability_up_tvalid) do
      cycle ()
    done;
    inputs.up.tdata := Bits.of_int ~width:16 x;
    inputs.up.tvalid := Bits.vdd;
    while
      cycle ();
      Bits.is_gnd !(outputs_before.up_dest.tready)
    do
      ()
    done);
  inputs.up.tvalid := Bits.gnd;
  for _ = 0 to List.length input_data * 2 do
    cycle ()
  done;
  if debug then Waveform.Serialize.marshall waves "a.hardcamlwaveform.Z";
  let output_data = Accumulator.dump accumulator in
  if not ([%equal: int list] input_data output_data)
  then
    raise_s
      [%message
        "Test failed!" (input_data : Int.Hex.t list) (output_data : Int.Hex.t list)]
;;

let%expect_test "Input line rate, output always ready" =
  test ~num_cycles:200 ~probability_dn_tready:1.0 ~probability_up_tvalid:1.0
;;

let%expect_test "Input 50% rate, output always ready" =
  test ~num_cycles:200 ~probability_dn_tready:0.5 ~probability_up_tvalid:1.0
;;

let%expect_test "Input line rate, output 50% ready" =
  test ~num_cycles:200 ~probability_dn_tready:1.0 ~probability_up_tvalid:0.5
;;

let%expect_test "Input 50% rate, output 50% ready" =
  test ~num_cycles:200 ~probability_dn_tready:0.5 ~probability_up_tvalid:0.5
;;
