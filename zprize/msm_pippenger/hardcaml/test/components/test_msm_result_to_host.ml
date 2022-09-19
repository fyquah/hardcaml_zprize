open Core
open Hardcaml
open Msm_pippenger
module Msm_result_to_host = Msm_result_to_host.Make (Config.Bls12_377)
module Sim = Cyclesim.With_interface (Msm_result_to_host.I) (Msm_result_to_host.O)
module Waveform = Hardcaml_waveterm.Waveform
module Xyzt = Msm_result_to_host.Mixed_add.Xyzt

let create_sim ~drop_t () =
  let scope = Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true () in
  let sim =
    Sim.create
      ~config:Cyclesim.Config.trace_all
      (Msm_result_to_host.hierarchical ~drop_t scope)
  in
  let i = Cyclesim.inputs sim in
  i.clear := Bits.vdd;
  Cyclesim.cycle sim;
  i.clear := Bits.gnd;
  Cyclesim.cycle sim;
  sim
;;

let random_points num_points =
  Array.init num_points ~f:(fun _idx ->
    { Xyzt.x = Bits.random ~width:Config.Bls12_377.field_bits
    ; Xyzt.y = Bits.random ~width:Config.Bls12_377.field_bits
    ; Xyzt.z = Bits.random ~width:Config.Bls12_377.field_bits
    ; Xyzt.t = Bits.random ~width:Config.Bls12_377.field_bits
    })
;;

let timeout = 1_000

let test ?(seed = 0) ~drop_t num_points =
  Random.init seed;
  let waves, sim =
    Waveform.create (create_sim ~drop_t:(if drop_t then Signal.vdd else Signal.gnd) ())
  in
  let i, o = Cyclesim.inputs sim, Cyclesim.outputs ~clock_edge:Before sim in
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  let data = ref [] in
  let cycle_cnt = ref 0 in
  let cycle () =
    Cyclesim.cycle sim;
    if Bits.is_vdd !(o.fpga_to_host.tvalid) && Bits.is_vdd !(i.fpga_to_host_dest.tready)
    then data := !(o.fpga_to_host.tdata) :: !data;
    (* Random backpressure *)
    i.fpga_to_host_dest.tready := Bits.random ~width:1;
    Int.incr cycle_cnt
  in
  let input_points = random_points num_points in
  for idx = 0 to num_points - 1 do
    i.result_point_valid := Bits.vdd;
    if idx = num_points - 1 then i.last_result_point := Bits.vdd;
    Xyzt.iter2 i.result_point input_points.(idx) ~f:( := );
    cycle ();
    while !cycle_cnt < timeout && Bits.is_gnd !(o.result_point_ready) do
      cycle ()
    done;
    i.result_point_valid := Bits.gnd;
    i.last_result_point := Bits.gnd
  done;
  cycle ();
  while
    !cycle_cnt < timeout
    && not
         (Bits.is_vdd !(o.fpga_to_host.tvalid)
         && Bits.is_vdd !(o.fpga_to_host.tlast)
         && Bits.is_vdd !(i.fpga_to_host_dest.tready))
  do
    cycle ()
  done;
  cycle ();
  let data = List.(rev !data |> to_array) in
  let alignment = 64 in
  let axi = 512 in
  let field_bits = Config.Bls12_377.field_bits in
  let aligned_field_bits = Int.round_up field_bits ~to_multiple_of:alignment in
  let num_words_per_point =
    Int.round_up ((if drop_t then 3 else 4) * aligned_field_bits) ~to_multiple_of:axi
    / axi
  in
  let data_as_points : Bits.t Xyzt.t array =
    Array.init num_points ~f:(fun idx ->
      let flat_data =
        Array.slice data (idx * num_words_per_point) ((idx + 1) * num_words_per_point)
        |> Bits.of_array
      in
      let x = Bits.select flat_data (field_bits - 1) 0 in
      let y =
        Bits.select flat_data (aligned_field_bits + field_bits - 1) aligned_field_bits
      in
      let z =
        Bits.select
          flat_data
          ((2 * aligned_field_bits) + field_bits - 1)
          (aligned_field_bits * 2)
      in
      let t =
        if drop_t
        then Bits.zero field_bits
        else
          Bits.select
            flat_data
            ((3 * aligned_field_bits) + field_bits - 1)
            (aligned_field_bits * 3)
      in
      { Xyzt.x; y; z; t })
  in
  let idx = ref 0 in
  Array.iter2_exn data_as_points input_points ~f:(fun fpga expected ->
    let expected =
      if drop_t
      then { expected with t = Bits.zero Config.Bls12_377.field_bits }
      else expected
    in
    Xyzt.(
      iter3 port_names fpga expected ~f:(fun n fpga expected ->
        if Bits.(is_gnd (fpga ==: expected))
        then
          print_s
            [%message
              "FAILURE - coordinate mismatch"
                (!idx : int)
                (n : string)
                (fpga : Bits.t)
                (expected : Bits.t)]));
    Int.incr idx);
  cycle ();
  cycle ();
  waves
;;

let%expect_test "Small test" =
  let _waves0 = test ~drop_t:true 5 in
  let _waves1 = test ~drop_t:false ~seed:1 5 in
  [%expect {| |}]
;;

let waveform _ = test ~drop_t:false ~seed:0 2
