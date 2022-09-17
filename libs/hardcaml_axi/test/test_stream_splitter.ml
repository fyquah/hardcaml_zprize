open Core
open Hardcaml
open Hardcaml_axi
open Import
module Waveform = Hardcaml_waveterm.Waveform

let num_outputs = 3

module Splitter =
  Stream_splitter.Make
    (Axi16.Stream)
    (struct
      let num_outputs = num_outputs
    end)

module Sim = Cyclesim.With_interface (Splitter.I) (Splitter.O)

let create_sim () =
  let scope = Scope.create ~flatten_design:true () in
  let sim = Sim.create (Splitter.create scope) in
  sim
;;

let debug = false

let test ~num_cycles ~probability_dns_tready ~probability_up_tvalid =
  let input_data = List.init num_cycles ~f:(fun _ -> Random.int (1 lsl 16)) in
  let waves, sim = Waveform.create (create_sim ()) in
  let inputs = Cyclesim.inputs sim in
  let outputs_before = Cyclesim.outputs ~clock_edge:Before sim in
  let accumulators = Array.init num_outputs ~f:(fun _ -> Accumulator.create ()) in
  let cycle () =
    Array.iteri inputs.dn_dests ~f:(fun i d ->
      d.tready := Bits.of_bool (random_bool ~p_true:probability_dns_tready.(i)));
    Cyclesim.cycle sim;
    for i = 0 to num_outputs - 1 do
      let dn_dest = inputs.dn_dests.(i) in
      let dn = outputs_before.dns.(i) in
      if Bits.to_bool !(dn.tvalid) && Bits.to_bool !(dn_dest.tready)
      then Accumulator.push accumulators.(i) (Bits.to_int !(dn.tdata))
    done
  in
  List.iter input_data ~f:(fun tdata ->
    inputs.up.tvalid := Bits.gnd;
    while not (random_bool ~p_true:probability_up_tvalid) do
      cycle ()
    done;
    inputs.up.tdata := Bits.of_int ~width:16 tdata;
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
  let output_datas = Array.map accumulators ~f:Accumulator.dump in
  if not (Array.for_all output_datas ~f:(fun d -> [%equal: int list] d input_data))
  then
    raise_s [%message (input_data : Int.Hex.t list) (output_datas : Int.Hex.t list array)]
;;

let%expect_test "line rate tvalid, always tready" =
  test
    ~num_cycles:1_000
    ~probability_up_tvalid:1.0
    ~probability_dns_tready:[| 1.0; 1.0; 1.0 |]
;;

let%expect_test "mixed tvalid, mixed tready" =
  test
    ~num_cycles:1_000
    ~probability_up_tvalid:0.9
    ~probability_dns_tready:[| 0.9; 0.9; 0.9 |]
;;
