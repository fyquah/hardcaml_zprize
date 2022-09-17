open Core
open Hardcaml
open Msm_pippenger
module Compact_stream = Compact_stream.Make (Config.Bls12_377)
module Sim = Cyclesim.With_interface (Compact_stream.I) (Compact_stream.O)
module Waveform = Hardcaml_waveterm.Waveform

(* CR-someday fquah: Share these and axi stream driving code from
 * Hardcaml_axi_test
 *)
module Accumulator = struct
  let create () = ref []
  let push l hd = l := hd :: !l
  let dump l = List.rev !l
end

let random_bool ~p_true = Float.(Random.float 1.0 < p_true)

let create_sim () =
  let scope = Scope.create ~flatten_design:true () in
  Sim.create ~config:Cyclesim.Config.trace_all (Compact_stream.create scope)
;;

let num_256_words_per_point = Compact_stream.num_256_words_per_point
let num_512_words_per_point = (num_256_words_per_point + 1) / 2

let%expect_test "" =
  print_s [%message (num_256_words_per_point : int) (num_512_words_per_point : int)];
  [%expect {| ((num_256_words_per_point 6) (num_512_words_per_point 3)) |}]
;;

let debug = false

module Input_data = struct
  type t =
    { data : Bits.t list
    ; last : bool
    }
  [@@deriving equal, sexp_of]
end

module Output_for_a_cycle = struct
  type t =
    { tdata : Bits.t
    ; tlast : bool
    }
  [@@deriving equal, sexp_of]
end

let test ~num_points ~probability_up_tvalid ~probability_dn_tready =
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
    then
      Accumulator.push
        accumulator
        { Output_for_a_cycle.tdata = !(outputs_before.dn.tdata)
        ; tlast = Bits.to_bool !(outputs_before.dn.tlast)
        }
  in
  let input_datas =
    List.init num_points ~f:(fun _ ->
      { Input_data.data =
          List.init num_512_words_per_point ~f:(fun _ -> Bits.random ~width:512)
      ; last = Random.bool ()
      })
  in
  inputs.up.tvalid := Bits.gnd;
  List.iter input_datas ~f:(fun point ->
    List.iteri point.data ~f:(fun i tdata ->
      while not (random_bool ~p_true:probability_up_tvalid) do
        cycle ()
      done;
      inputs.up.tdata := tdata;
      inputs.up.tvalid := Bits.vdd;
      inputs.up.tlast := Bits.of_bool (i = List.length point.data - 1 && point.last);
      while
        cycle ();
        Bits.is_gnd !(outputs_before.up_dest.tready)
      do
        ()
      done;
      inputs.up.tvalid := Bits.gnd));
  for _ = 0 to 10 do
    cycle ()
  done;
  let expected_output_data =
    List.concat_map input_datas ~f:(fun v ->
      List.concat_mapi v.data ~f:(fun i x ->
        let btm = Bits.sel_bottom x 256 in
        let top = Bits.sel_top x 256 in
        if i = num_512_words_per_point - 1
        then
          (* This is the last 512-bit word that needs to be transmitted. *)
          if num_256_words_per_point % 2 = 0
          then
            (* We need both words to fit 256-bit words, so transmit both. Only
             * the last cycle will have tlast. *)
            [ { Output_for_a_cycle.tdata = btm; tlast = false }
            ; { Output_for_a_cycle.tdata = top; tlast = v.last }
            ]
          else
            (* We only need to transmit the bottom 256 bit words. assert tlast
             * appropriately.
             *)
            [ { Output_for_a_cycle.tdata = btm; tlast = v.last } ]
        else
          [ { Output_for_a_cycle.tdata = btm; tlast = false }
          ; { Output_for_a_cycle.tdata = top; tlast = false }
          ]))
  in
  let obtained_output_data = Accumulator.dump accumulator in
  if debug then Waveform.Serialize.marshall waves "a.hardcamlwaveform";
  if not ([%equal: Output_for_a_cycle.t list] obtained_output_data expected_output_data)
  then
    raise_s
      [%message
        "Data mismatch"
          (input_datas : Input_data.t list)
          (List.zip_exn expected_output_data obtained_output_data
            : (Output_for_a_cycle.t * Output_for_a_cycle.t) list)]
;;

let%expect_test "tvalid always high, tready always high" =
  test ~num_points:100 ~probability_up_tvalid:1.0 ~probability_dn_tready:1.0
;;

let%expect_test "tvalid always high, tready 90%" =
  test ~num_points:100 ~probability_up_tvalid:1.0 ~probability_dn_tready:0.9
;;

let%expect_test "tvalid 90%, tready always high" =
  test ~num_points:100 ~probability_up_tvalid:0.9 ~probability_dn_tready:1.0
;;
