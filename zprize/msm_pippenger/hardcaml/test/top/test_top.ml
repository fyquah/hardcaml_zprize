open Core
open Hardcaml
open Hardcaml_waveterm
open Msm_pippenger

module Make (Config : Msm_pippenger.Config.S) = struct
  module Utils = Utils.Make (Config)
  module Config_utils = Config_utils.Make (Config)
  module Top = Top.Make (Config)
  module I = Top.I
  module O = Top.O
  module Sim = Cyclesim.With_interface (I) (O)
  module I_rules = Display_rules.With_interface (Top.I)
  module O_rules = Display_rules.With_interface (Top.O)

  let precompute = Top.precompute

  let create_sim verilator () =
    let scope =
      Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ()
    in
    let create = Top.hierarchical ~build_mode:Simulation scope in
    if verilator
    then
      let module V = Hardcaml_verilator.With_interface (Top.I) (Top.O) in
      V.create ~clock_names:[ "clock" ] ~cache_dir:"/tmp/top/" ~verbose:true create
    else
      Sim.create
        ~config:{ Cyclesim.Config.trace_all with deduplicate_signals = false }
        create
  ;;

  type result =
    { waves : Waveform.t
    ; inputs : Bits.t Utils.Msm_input.t array
    ; points : Utils.window_bucket_point list
    }

  let run_test ?(seed = 0) ?(timeout = 10_000) ?(verilator = false) num_inputs =
    let cycle_cnt = ref 0 in
    let reset_cycle_cnt () =
      if !cycle_cnt = timeout then print_s [%message "ERROR: Timeout during simulation!"];
      cycle_cnt := 0
    in
    let sim = create_sim verilator () in
    let waves, sim = Waveform.create sim in
    let i, o = Cyclesim.inputs sim, Cyclesim.outputs sim in
    i.clear := Bits.vdd;
    Cyclesim.cycle sim;
    i.clear := Bits.gnd;
    Cyclesim.cycle sim;
    reset_cycle_cnt ();
    let inputs =
      Utils.random_inputs
        ~precompute
        ~seed
        num_inputs
        ~top_window_size:Config_utils.top_window_size
    in
    Array.iteri inputs ~f:(fun idx input ->
      Top.Mixed_add.Xyt.iter2
        i.input_point
        { x = input.affine_point_with_t.x
        ; y = input.affine_point_with_t.y
        ; t = input.affine_point_with_t.t
        }
        ~f:( := );
      i.scalar := Utils.convert_scalar input.scalar;
      i.scalar_valid := Bits.vdd;
      if idx = num_inputs - 1 then i.last_scalar := Bits.vdd;
      while Bits.is_gnd !(o.scalar_and_input_point_ready) && !cycle_cnt < timeout do
        Int.incr cycle_cnt;
        Cyclesim.cycle sim
      done;
      Cyclesim.cycle sim);
    i.scalar_valid := Bits.gnd;
    reset_cycle_cnt ();
    let result_points = ref [] in
    let result_point_cnt = ref 0 in
    while Bits.is_gnd !(o.result_point_valid) && !cycle_cnt < timeout do
      Cyclesim.cycle sim;
      Int.incr cycle_cnt
    done;
    i.result_point_ready := Bits.vdd;
    reset_cycle_cnt ();
    let bucket = ref (Config_utils.num_buckets 0) in
    let window = ref 0 in
    print_s [%message "Expecting" (Top.num_result_points : int)];
    while !result_point_cnt < Top.num_result_points && !cycle_cnt < timeout do
      if Bits.is_vdd !(o.result_point_valid)
      then (
        let result_point =
          let to_z b = Bits.to_constant !b |> Constant.to_z ~signedness:Unsigned in
          { Utils.Extended.x = to_z o.result_point.x
          ; y = to_z o.result_point.y
          ; z = to_z o.result_point.z
          ; t = to_z o.result_point.t
          }
        in
        let extended : Utils.Twisted_edwards.extended =
          { x = result_point.x
          ; y = result_point.y
          ; t = result_point.t
          ; z = result_point.z
          }
        in
        let affine = Utils.twisted_edwards_extended_to_affine ~precompute extended in
        result_points
          := { Utils.point = affine; bucket = !bucket; window = !window }
             :: !result_points;
        Int.incr result_point_cnt;
        bucket
          := if !bucket = 1
             then (
               Int.incr window;
               Config_utils.num_buckets !window)
             else !bucket - 1);
      Cyclesim.cycle sim;
      Int.incr cycle_cnt
    done;
    print_s [%message "Got" (List.length !result_points : int)];
    [%test_result: int] ~expect:Top.num_result_points (List.length !result_points);
    reset_cycle_cnt ();
    Cyclesim.cycle sim;
    let result = { waves; points = List.rev !result_points; inputs } in
    let fpga_calculated_result = Utils.calculate_result_from_fpga result.points in
    let expected = Utils.expected result.inputs in
    if not (Ark_bls12_377_g1.equal_affine fpga_calculated_result expected)
    then
      print_s
        [%message
          "ERROR: Result points did not match!"
            (expected : Ark_bls12_377_g1.affine)
            (fpga_calculated_result : Ark_bls12_377_g1.affine)]
    else print_s [%message "PASS"];
    result
  ;;
end

let display_rules =
  [ Display_rule.port_name_matches
      ~wave_format:Int
      (Re.compile (Re.Perl.re ".*adder.*p1.*"))
  ; Display_rule.default
  ]
;;

let%expect_test "Test over small input size and small number of scalars" =
  let module Config = struct
    let field_bits = 377
    let scalar_bits = 9
    let controller_log_stall_fifo_depth = 2
    let num_windows = 4
    let window_ram_partition_settings = None
  end
  in
  let module Test = Make (Config) in
  let result = Test.run_test 1 in
  print_s [%message (result.points : Test.Utils.window_bucket_point list)];
  [%expect
    {|
    (Expecting (Top.num_result_points 11))
    (Got ("List.length (!result_points)" 11))
    PASS
    (result.points
     (((point ()) (bucket 4) (window 0)) ((point ()) (bucket 3) (window 0))
      ((point
        (((x
           0x15519d706bc4a7f8f5040b7a0fed1c0bf6b5352c982a43a2b4cb2873efb4ed423c42771548e4a42104e7eed7001a701)
          (y
           0x6fc267222a824a554287790181ab66409855e518b3eb724187c0a643fb5a4661146d32e8b3baf700a8fed07e67bcfc))))
       (bucket 2) (window 0))
      ((point ()) (bucket 1) (window 0))
      ((point
        (((x
           0x15519d706bc4a7f8f5040b7a0fed1c0bf6b5352c982a43a2b4cb2873efb4ed423c42771548e4a42104e7eed7001a701)
          (y
           0x13e77def59a8ea070f87e476b1f9dd4d98a840de841281cdd6ba189760dedb9b5f6f011474c4509845fc12f81984305))))
       (bucket 2) (window 1))
      ((point ()) (bucket 1) (window 1)) ((point ()) (bucket 2) (window 2))
      ((point
        (((x
           0x15519d706bc4a7f8f5040b7a0fed1c0bf6b5352c982a43a2b4cb2873efb4ed423c42771548e4a42104e7eed7001a701)
          (y
           0x13e77def59a8ea070f87e476b1f9dd4d98a840de841281cdd6ba189760dedb9b5f6f011474c4509845fc12f81984305))))
       (bucket 1) (window 2))
      ((point ()) (bucket 3) (window 3)) ((point ()) (bucket 2) (window 3))
      ((point
        (((x
           0x15519d706bc4a7f8f5040b7a0fed1c0bf6b5352c982a43a2b4cb2873efb4ed423c42771548e4a42104e7eed7001a701)
          (y
           0x6fc267222a824a554287790181ab66409855e518b3eb724187c0a643fb5a4661146d32e8b3baf700a8fed07e67bcfc))))
       (bucket 1) (window 3)))) |}]
;;
