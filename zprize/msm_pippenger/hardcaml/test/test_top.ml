open Core
open Hardcaml
open Hardcaml_waveterm
open Msm_pippenger

module Config = struct
  let field_bits = 377
  let scalar_bits = 5
  let controller_log_stall_fifo_depth = 2
  let window_size_bits = 2
  let ram_read_latency = 1
end

module Top = Top.Make (Config)
module I = Top.I
module O = Top.O
module Sim = Cyclesim.With_interface (I) (O)
module I_rules = Display_rules.With_interface (Top.I)
module O_rules = Display_rules.With_interface (Top.O)

let create_sim () =
  let scope = Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true () in
  Sim.create
    ~config:Cyclesim.Config.trace_all
    (Top.hierarchical ~build_mode:Simulation scope)
;;

let num_inputs = 8

module Affine_point_with_t = struct
  type 'a t =
    { x : 'a [@bits Config.field_bits]
    ; y : 'a [@bits Config.field_bits]
    ; t : 'a [@bits Config.field_bits]
    }
  [@@deriving sexp_of, hardcaml]
end

module Extended = struct
  type 'a t =
    { x : 'a [@bits Config.field_bits]
    ; y : 'a [@bits Config.field_bits]
    ; z : 'a [@bits Config.field_bits]
    ; t : 'a [@bits Config.field_bits]
    }
  [@@deriving sexp_of, hardcaml]
end

module Msm_input = struct
  type 'a t =
    { scalar : 'a [@bits Config.scalar_bits]
    ; affine_point_with_t : 'a Affine_point_with_t.t
    }
  [@@deriving sexp_of, hardcaml]
end

let random_inputs () =
  Random.init 0;
  Array.init num_inputs ~f:(fun _ ->
    let affine_point =
      Ark_bls12_377_g1.(mul (subgroup_generator ()) ~by:(Random.int 100))
    in
    let affine_point_with_t =
      Twisted_edwards_model_lib.Twisted_edwards_curve.affine_to_affine_with_t
        { x = Ark_bls12_377_g1.x affine_point; y = Ark_bls12_377_g1.y affine_point }
    in
    { Msm_input.scalar = Bits.random ~width:Config.scalar_bits
    ; affine_point_with_t =
        { x = Bits.of_z ~width:Config.field_bits affine_point_with_t.x
        ; y = Bits.of_z ~width:Config.field_bits affine_point_with_t.y
        ; t = Bits.of_z ~width:Config.field_bits affine_point_with_t.t
        }
    })
;;

let timeout = 1_000

let run_small_test () =
  let cycle_cnt = ref 0 in
  let sim = create_sim () in
  let waves, sim = Waveform.create sim in
  let i, o = Cyclesim.inputs sim, Cyclesim.outputs sim in
  i.clear := Bits.vdd;
  Cyclesim.cycle sim;
  i.clear := Bits.gnd;
  i.start := Bits.vdd;
  Cyclesim.cycle sim;
  i.start := Bits.gnd;
  cycle_cnt := 0;
  Array.iteri (random_inputs ()) ~f:(fun idx input ->
    i.input_point := Affine_point_with_t.Of_bits.pack input.affine_point_with_t;
    i.scalar := input.scalar;
    i.scalar_valid := Bits.vdd;
    if idx = num_inputs - 1 then i.last_scalar := Bits.vdd;
    while Bits.is_gnd !(o.scalar_and_input_point_ready) && !cycle_cnt < timeout do
      Int.incr cycle_cnt;
      Cyclesim.cycle sim
    done;
    Cyclesim.cycle sim);
  i.scalar_valid := Bits.gnd;
  cycle_cnt := 0;
  let result_points = ref [] in
  while
    Bits.is_gnd !(o.result_point_valid) && Bits.is_gnd !(o.error) && !cycle_cnt < timeout
  do
    Cyclesim.cycle sim;
    Int.incr cycle_cnt
  done;
  i.result_point_ready := Bits.vdd;
  cycle_cnt := 0;
  print_s [%message "Expecting" (Top.num_result_points : int)];
  while List.length !result_points < Top.num_result_points && !cycle_cnt < timeout do
    if Bits.is_vdd !(o.result_point_valid)
    then (
      let result_point = Extended.Of_bits.unpack !(o.result_point) in
      result_points := result_point :: !result_points;
      Cyclesim.cycle sim);
    Int.incr cycle_cnt
  done;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  waves
;;

let display_rules =
  List.concat
    [ I_rules.default ()
    ; O_rules.default ()
    ; [ Display_rule.port_name_is "STATE" ~wave_format:(Index Top.State.names)
      ; Display_rule.default
      ]
    ]
;;

let%expect_test "Test over small input size" =
  let waves = run_small_test () in
  Waveform.expect ~display_width:50 ~display_height:40 ~display_rules waves;
  [%expect {||}]
;;

let waveform () = run_small_test ()
