open Core
open Hardcaml
module Waveform = Hardcaml_waveterm.Waveform
module Stream = Hardcaml_axi.Axi16.Stream
module Sim = Cyclesim.With_interface (Stream.Register.I) (Stream.Register.O)

module Accumulator = struct
  let create () = ref []
  let push l hd = l := hd :: !l
  let dump l = List.rev !l
end

let create_sim () =
  let scope = Scope.create ~flatten_design:true () in
  Sim.create (Stream.Register.create scope)
;;

let%expect_test "" =
  let waves, sim = Waveform.create (create_sim ()) in
  let inputs = Cyclesim.inputs sim in
  let outputs_before = Cyclesim.outputs ~clock_edge:Before sim in
  inputs.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clear := Bits.gnd;
  let accumulator = Accumulator.create () in
  let cycle () =
    inputs.dn_dest.tready := Bits.of_int ~width:1 (Bool.to_int (Random.bool ()));
    Cyclesim.cycle sim;
    if Bits.to_bool !(inputs.dn_dest.tready) && Bits.to_bool !(outputs_before.dn.tvalid)
    then Accumulator.push accumulator (Bits.to_int !(outputs_before.dn.tdata))
    else
      Stdio.printf
        "tready= %d, tvalid = %d\n"
        (Bits.to_int !(inputs.dn_dest.tready))
        (Bits.to_int !(outputs_before.dn.tvalid))
  in
  let input_data = [ 0xAB; 0xCD; 0xEF ] in
  List.iter input_data ~f:(fun x ->
    (* Cycle for awhile, just because *)
    inputs.up.tvalid := Bits.gnd;
    while Random.bool () do
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
  let output_data = Accumulator.dump accumulator in
  for _ = 0 to List.length input_data * 2 do
    cycle ()
  done;
  Waveform.Serialize.marshall waves "a.hardcamlwaveform.Z";
  if not ([%equal: int list] input_data output_data)
  then raise_s [%message "Test failed!" (input_data : int list) (output_data : int list)]
;;
