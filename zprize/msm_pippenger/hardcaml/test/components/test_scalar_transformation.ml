open Core
open Hardcaml
open Msm_pippenger
module Config = Config.Bls12_377

module Scalar_transformation =
  Scalar_transformation.Make
    (Config)
    (struct
      let num_bits = Config.field_bits
    end)

open Config_utils.Make (Config)

include struct
  open Msm_pippenger_test_top.Utils.Make (Config)
  module Reduced_scalar = Reduced_scalar

  let model = perform_scalar_reduction
  let model_check = check_scalar_reduction
end

module Sim = Cyclesim.With_interface (Scalar_transformation.I) (Scalar_transformation.O)
module Waveform = Hardcaml_waveterm.Waveform

let create_sim () =
  let scope = Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true () in
  Sim.create ~config:Cyclesim.Config.trace_all (Scalar_transformation.create scope)
;;

let test_scalars =
  List.init
    (1 lsl window_bit_sizes.(0))
    ~f:(fun i ->
      let base_scalar = Bits.of_int ~width:window_bit_sizes.(0) i in
      List.init 100 ~f:(fun _ ->
        let top_width = window_bit_sizes.(num_windows - 1) in
        let top =
          Random.int_incl 0 ((1 lsl top_width) - 2) |> Bits.of_int ~width:top_width
        in
        Bits.(
          top
          @: random ~width:(Config.scalar_bits - window_bit_sizes.(0) - top_width)
          @: base_scalar)))
  |> List.concat
;;

let%expect_test "Check some scalars with the model" =
  List.iter test_scalars ~f:(fun s ->
    let _res = model s in
    ());
  print_s [%message "Checked" (List.length test_scalars : int)];
  [%expect {| (Checked ("List.length test_scalars" 819200)) |}]
;;

let test ?(verify = true) () =
  (* just keep driving inputs through, wiggling tready, until we have gotten all the outputs out *)
  (* create sim *)
  let waves, sim = Waveform.create (create_sim ()) in
  let i, o = Cyclesim.inputs sim, Cyclesim.outputs sim in
  i.clear := Bits.vdd;
  Cyclesim.cycle sim;
  i.clear := Bits.gnd;
  (* cycle *)
  let timeout = 1000 in
  let num_inputs = 20 in
  let rec run cycle_cnt tvalid inputs outputs =
    if cycle_cnt > timeout
    then (
      (* timed out or received all the outputs *)
      print_s [%message "Timed out!" (num_inputs : int) (List.length outputs : int)];
      outputs)
    else if List.length outputs = num_inputs
    then (
      print_s [%message "Completed!"];
      outputs)
    else (
      (* deal with downstream *)
      let tready = Bits.random ~width:1 in
      i.scalar_and_input_point_ready := tready;
      let outputs =
        if Bits.(is_vdd (!(o.scalar_valid) &: tready))
        then (
          (* we need to save the output *)
          let out =
            Array.map2_exn o.scalar o.scalar_negatives ~f:(fun s n ->
              { Reduced_scalar.scalar = !s; negative = Bits.is_vdd !n })
          in
          out :: outputs)
        else outputs
      in
      (* deal with upstream *)
      match inputs with
      | [] ->
        i.scalar_valid := Bits.gnd;
        cycle cycle_cnt Bits.gnd [] outputs
      | hd :: tl ->
        let tvalid = Bits.(tvalid |: random ~width:1) in
        let tlast = Bits.of_bool (List.length tl = 0) in
        i.scalar := hd;
        i.scalar_valid := tvalid;
        i.last_scalar := tlast;
        if Bits.(is_vdd (!(o.scalar_and_input_point_ready) &: tvalid))
        then cycle cycle_cnt Bits.gnd tl outputs
        else cycle cycle_cnt tvalid inputs outputs)
  and cycle cycle_cnt tvalid inputs outputs =
    Cyclesim.cycle sim;
    run (cycle_cnt + 1) tvalid inputs outputs
  in
  let inputs = List.take test_scalars 20 in
  let outputs = run 0 Bits.gnd inputs [] in
  if verify
  then (
    List.iter2_exn inputs outputs ~f:(fun i o -> model_check i o);
    print_s [%message "Checked" (num_inputs : int)]);
  waves
;;

let waveform ?verify () = test ?verify ()

let%expect_test "Drive inputs through transform" =
  let _waves = test () in
  [%expect]
;;
