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
module Twisted_edwards = Twisted_edwards_model_lib.Twisted_edwards_curve

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
      Twisted_edwards.affine_to_affine_with_t
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

type result =
  { waves : Waveform.t
  ; affine_points : Twisted_edwards.affine list
  }

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
  while Bits.is_gnd !(o.result_point_valid) && !cycle_cnt < timeout do
    Cyclesim.cycle sim;
    Int.incr cycle_cnt
  done;
  i.result_point_ready := Bits.vdd;
  cycle_cnt := 0;
  print_s [%message "Expecting" (Top.num_result_points : int)];
  while List.length !result_points < Top.num_result_points && !cycle_cnt < timeout do
    if Bits.is_vdd !(o.result_point_valid)
    then (
      let result_point =
        Extended.(
          Of_bits.unpack !(o.result_point)
          |> map ~f:(fun b -> Bits.to_constant b |> Constant.to_z ~signedness:Unsigned))
      in
      let extended : Twisted_edwards.extended =
        { x = result_point.x; y = result_point.y; t = result_point.t; z = result_point.z }
      in
      let affine = Twisted_edwards.extended_to_affine extended in
      result_points := affine :: !result_points;
      Cyclesim.cycle sim);
    Int.incr cycle_cnt
  done;
  Cyclesim.cycle sim;
  { waves; affine_points = !result_points }
;;

let display_rules =
  [ Display_rule.port_name_matches
      ~wave_format:Int
      (Re.compile (Re.Perl.re ".*adder.*p1.*"))
  ; Display_rule.default
  ]
;;

let%expect_test "Test over small input size" =
  let result = run_small_test () in
  print_s [%message (result.affine_points : Twisted_edwards.affine list)];
  (* Waveform.expect ~display_width:50 ~display_height:40 ~display_rules result.waves; *)
  [%expect
    {|
    (Expecting (Top.num_result_points 10))
    (result.affine_points
     (((x 0x0) (y 0x1))
      ((x
        0xa22f83ef6884e1e262c53f97c02cec0bde1d3200281c010e7865ea2aed462af916920936a7c93a2693d7b8d6cfb698)
       (y
        0x147b06316394fe4ac0763eb298463f33e454cefa1de9982fd76004cf9009843d981dcc9eb75aa5739cb0043266006ba))
      ((x 0x0) (y 0x1))
      ((x
        0x7f9bcec66ae4329145f884532d571dc89907932d40969ea9d8e938cb556c8da6262656ccf05b825b7cc27023f8c223)
       (y
        0x1712d868eba696f8298f526035adbbf34cf08e876f93ccaf998934a1d4d21fdab9b22852f332ee1bf2461d7b1f23f1))
      ((x
        0xeda6374751771af5ec644b60cd31db2f0da9e786b38216736243f6d7d8dbb9b725fcc0883e0cb65382ba31f6c360ac)
       (y
        0xacdeeb4376c8117a2db074eee02c2dc0bce7a93a95d591b4a6d6f91e32a46ed3258c61438447ca3653bd5f7aa22194))
      ((x 0x0) (y 0x1))
      ((x
        0x16c861d1777513f23e55ec932ae619f551c55320fe59bceb25f5ccd384e2b9e0abc054f0f7c520eccf18a087be28401)
       (y
        0x83b27e226f31081cb231c1494b547f70526dcc0444cbe807b885dc3be03f3c97426cf2eb99619a9a5d981f88dc8715))
      ((x
        0x14a9912a10c12f1de008099f921014106df1454b465ccdb3786f9de6282e8c295fb7e9b3a6bf2177d0b526f193209e2)
       (y
        0x1364da7c85bfdc87c1fd4b823260d92335a239150f4874952e567131d52650fc8b57113fbb552a63634c5f2cd9b951))
      ((x
        0x16c861d1777513f23e55ec932ae619f551c55320fe59bceb25f5ccd384e2b9e0abc054f0f7c520eccf18a087be28401)
       (y
        0x83b27e226f31081cb231c1494b547f70526dcc0444cbe807b885dc3be03f3c97426cf2eb99619a9a5d981f88dc8715))
      ((x
        0x15600c3f7cf8593e20f96ee20364d9139ff3b230036c7e881858ddada6f8f21f190d57d77e6cbe799e38e0a512bb5c1)
       (y
        0x122ce8777ad84e287c5c217b822cf8b4f3a3614a6b2462e1c2bdcbec2474dddc70e8bb20b459bf1e72cec16c8a2a7f0)))) |}]
;;

let waveform () = (run_small_test ()).waves
