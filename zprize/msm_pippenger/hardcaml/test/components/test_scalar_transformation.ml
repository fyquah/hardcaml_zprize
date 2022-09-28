open Core
open Hardcaml
open Msm_pippenger
(*module Config = Config.Bls12_377*)

module Config = struct
  include Config.Bls12_377

  (*let scalar_bits = 24*)
end

module Scalar_transformation =
  Scalar_transformation.Make
    (Config)
    (struct
      let num_bits = Config.field_bits
    end)

open Config_utils.Make (Config)

module Z = struct
  include Z

  let _sexp_of_t t = Sexp.Atom ("0x" ^ Z.format "x" t)
end

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

let () = Random.init 0

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
        let middle_width = Config.scalar_bits - window_bit_sizes.(0) - top_width in
        if middle_width = 0
        then Bits.(top @: base_scalar)
        else Bits.(top @: random ~width:middle_width @: base_scalar)))
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
  let () = Random.init 0 in
  (* just keep driving inputs through, wiggling tready, until we have gotten all the outputs out *)
  (* create sim *)
  let waves, sim = Waveform.create (create_sim ()) in
  let i, o = Cyclesim.inputs sim, Cyclesim.outputs ~clock_edge:Before sim in
  i.clear := Bits.vdd;
  Cyclesim.cycle sim;
  i.clear := Bits.gnd;
  (* cycle *)
  let timeout = 100000 in
  let num_inputs = ref 0 in
  let rec run cycle_cnt tvalid_prev inputs outputs =
    if cycle_cnt > timeout
    then (
      (* timed out or received all the outputs *)
      print_s [%message "Timed out!" (!num_inputs : int) (List.length outputs : int)];
      List.rev outputs)
    else if List.length outputs = !num_inputs
    then (
      print_s [%message "Completed!"];
      List.rev outputs)
    else (
      (* deal with downstream *)
      let tready = Bits.random ~width:1 in
      i.scalar_and_input_point_ready := tready;
      (* deal with upstream *)
      match inputs with
      | [] ->
        i.scalar_valid := Bits.gnd;
        cycle cycle_cnt Bits.gnd [] outputs
      | hd :: tl ->
        let tvalid = Bits.(tvalid_prev |: random ~width:1) in
        let tlast = Bits.of_bool (List.length tl = 0) in
        i.scalar := hd;
        i.scalar_valid := tvalid;
        i.last_scalar := tlast;
        cycle cycle_cnt tvalid inputs outputs)
  and cycle cycle_cnt tvalid inputs outputs =
    (*if cycle_cnt < 80
    then (
      print_s [%message (cycle_cnt : int)];
      Scalar_transformation.I.(
        iter2 i port_names ~f:(fun v name ->
          if not (String.is_substring name ~substring:"xyt")
          then print_s [%message (name : string) (!v : Bits.t)]));
      Scalar_transformation.O.(
        iter2 o port_names ~f:(fun v name ->
          if not (String.is_substring name ~substring:"xyt")
          then print_s [%message (name : string) (!v : Bits.t)])));*)
    Cyclesim.cycle sim;
    let cycle_cnt = cycle_cnt + 1 in
    let outputs =
      if Bits.(is_vdd (!(o.scalar_valid) &: !(i.scalar_and_input_point_ready)))
      then (
        (* we need to save the output *)
        let out =
          Array.map2_exn o.scalar o.scalar_negatives ~f:(fun s n ->
            { Reduced_scalar.scalar = !s; negative = Bits.is_vdd !n })
        in
        out :: outputs)
      else outputs
    in
    if Bits.(is_vdd (!(o.scalar_and_input_point_ready) &: !(i.scalar_valid)))
    then (
      let tl = List.tl_exn inputs in
      (*let hd = List.hd_exn inputs in*)
      (*let int_hd = Bits.(uresize hd (width hd + 1) |> to_z ~signedness:Unsigned) in*)
      run cycle_cnt Bits.gnd tl outputs)
    else run cycle_cnt tvalid inputs outputs
  in
  (*let slice l start len = List.drop (List.take l (start + len)) start in
  let inputs = slice test_scalars (100 * 2049) 20 in*)
  let inputs = List.filter test_scalars ~f:(fun _ -> Float.(Random.float 1. < 0.01)) in
  num_inputs := List.length inputs;
  let outputs = run 1 Bits.gnd inputs [] in
  (*print_s [%message (outputs : Reduced_scalar.t array list)];*)
  if verify
  then (
    List.iteri (List.zip_exn inputs outputs) ~f:(fun _idx (i, o) -> model_check i o);
    print_s [%message "Checked" (!num_inputs : int)]);
  waves
;;

let waveform ?verify () = test ?verify ()

let%expect_test "Drive inputs through transform" =
  let _waves = test () in
  [%expect{|
    Completed!
    (Checked (!num_inputs 8103)) |}]
;;
