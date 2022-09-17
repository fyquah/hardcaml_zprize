open Core
open Hardcaml
open Hardcaml_waveterm
open Msm_pippenger

let random_bool ~p_true = Float.(Random.float 1.0 < p_true)

module Make (Config : Msm_pippenger.Config.S) = struct
  module Utils = Utils.Make (Config)
  module Top = Top.Make (Config)
  module Kernel = Kernel_for_vitis.Make (Config)
  module I = Kernel.I
  module O = Kernel.O
  module I_rules = Display_rules.With_interface (Kernel.I)
  module O_rules = Display_rules.With_interface (Kernel.O)
  module Sim = Cyclesim.With_interface (I) (O)

  let create_sim verilator () =
    let scope =
      Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ()
    in
    let create = Kernel.hierarchical ~build_mode:Simulation scope in
    if verilator
    then
      let module V = Hardcaml_verilator.With_interface (Kernel.I) (Kernel.O) in
      V.create ~clock_names:[ "clock" ] ~cache_dir:"/tmp/kernel/" ~verbose:true create
    else Sim.create ~config:Cyclesim.Config.trace_all create
  ;;

  let display_rules =
    List.concat
      [ I_rules.default ()
      ; O_rules.default ()
      ; [ Display_rule.port_name_is "STATE" ~wave_format:(Index Kernel.State.names)
        ; Display_rule.default
        ; Display_rule.port_name_matches ~wave_format:Int (Re.compile (Re.Perl.re ".*"))
        ]
      ]
  ;;

  let num_clocks_per_input =
    Int.round_up (Config.scalar_bits + (3 * Config.field_bits)) ~to_multiple_of:512 / 512
  ;;

  let num_clocks_per_output =
    Int.round_up (4 * Config.field_bits) ~to_multiple_of:512 / 512
  ;;

  type result =
    { waves : Waveform.t option
    ; points : Utils.window_bucket_point list
    ; inputs : Bits.t Utils.Msm_input.t array
    }

  type sim_and_waves =
    { sim : Sim.t
    ; waves : Waveform.t option
    }

  let create ~verilator ~waves =
    let sim = create_sim verilator () in
    let i = Cyclesim.inputs sim in
    i.ap_rst_n := Bits.gnd;
    Cyclesim.cycle sim;
    Cyclesim.cycle sim;
    i.ap_rst_n := Bits.vdd;
    Cyclesim.cycle sim;
    Cyclesim.cycle sim;
    Cyclesim.cycle sim;
    if waves
    then (
      let waves, sim = Waveform.create sim in
      { waves = Some waves; sim })
    else { waves = None; sim }
  ;;

  let run
    ?sim
    ?(waves = true)
    ?(tvalid_probability = 0.1)
    ?(probability_tready = 0.1)
    ~seed
    ~timeout
    ~verilator
    num_inputs
    ()
    =
    let cycle_cnt = ref 0 in
    let sim_and_waves = Option.value sim ~default:(create ~verilator ~waves) in
    let sim = sim_and_waves.sim in
    let i, o = Cyclesim.inputs sim, Cyclesim.outputs sim in
    let inputs = Utils.random_inputs ~seed num_inputs in
    Cyclesim.cycle sim;
    i.fpga_to_host_dest.tready := Bits.vdd;
    for idx = 0 to num_inputs - 1 do
      let input = inputs.(idx) in
      let to_send =
        ref
          Bits.(
            input.scalar
            @: Utils.Affine_point_with_t.Of_bits.pack input.affine_point_with_t)
      in
      for beat = 0 to num_clocks_per_input - 1 do
        i.host_to_fpga.tdata := Bits.sel_bottom !to_send 512;
        while not (random_bool ~p_true:tvalid_probability) do
          i.host_to_fpga.tvalid := Bits.gnd;
          i.host_to_fpga.tlast := Bits.random ~width:1;
          Cyclesim.cycle sim
        done;
        i.host_to_fpga.tlast
          := Bits.of_bool (beat = num_clocks_per_input - 1 && idx = num_inputs - 1);
        i.host_to_fpga.tvalid := Bits.vdd;
        cycle_cnt := 0;
        while Bits.is_gnd !(o.host_to_fpga_dest.tready) && !cycle_cnt < timeout do
          Int.incr cycle_cnt;
          Cyclesim.cycle sim
        done;
        Cyclesim.cycle sim;
        to_send := Bits.(srl !to_send 512)
      done;
      i.host_to_fpga.tvalid := Bits.gnd;
      for _ = 0 to Random.int 5 do
        Cyclesim.cycle sim
      done
    done;
    i.host_to_fpga.tlast := Bits.gnd;
    i.host_to_fpga.tvalid := Bits.gnd;
    Cyclesim.cycle sim;
    cycle_cnt := 0;
    while Bits.is_gnd !(o.fpga_to_host.tvalid) && !cycle_cnt < timeout do
      Int.incr cycle_cnt;
      Cyclesim.cycle sim
    done;
    cycle_cnt := 0;
    let word = ref 0 in
    let output_buffer_bits = num_clocks_per_output * 512 in
    let output_buffer = ref (Bits.zero output_buffer_bits) in
    let result_points = ref [] in
    let bucket = ref ((1 lsl Config.window_size_bits) - 1) in
    let window = ref 0 in
    let is_last = ref false in
    print_s [%message "Expecting" (Top.num_result_points : int)];
    while (not !is_last) && !cycle_cnt < timeout do
      i.fpga_to_host_dest.tready := Bits.of_bool (random_bool ~p_true:probability_tready);
      Int.incr cycle_cnt;
      if Bits.is_vdd !(i.fpga_to_host_dest.tready) && Bits.is_vdd !(o.fpga_to_host.tvalid)
      then (
        (output_buffer
           := Bits.(
                sel_top (!(o.fpga_to_host.tdata) @: !output_buffer) output_buffer_bits));
        Int.incr word;
        if !word = num_clocks_per_output
        then (
          word := 0;
          is_last := Bits.to_bool !(o.fpga_to_host.tlast);
          let to_z b = Bits.to_constant b |> Constant.to_z ~signedness:Unsigned in
          let result_point = Utils.Extended.Of_bits.unpack !output_buffer in
          let extended : Utils.Twisted_edwards.extended =
            { x = to_z result_point.x
            ; y = to_z result_point.y
            ; t = to_z result_point.t
            ; z = to_z result_point.z
            }
          in
          let affine = Utils.twisted_edwards_extended_to_affine extended in
          result_points
            := { Utils.point = affine; bucket = !bucket; window = !window }
               :: !result_points;
          bucket
            := if !bucket = 1
               then (
                 Int.incr window;
                 let next_window_size_bits =
                   if !window = Top.num_windows - 1
                   then Top.last_window_size_bits
                   else Config.window_size_bits
                 in
                 (1 lsl next_window_size_bits) - 1)
               else !bucket - 1));
      Cyclesim.cycle sim
    done;
    print_s [%message "Got" (List.length !result_points : int)];
    let result =
      { waves = sim_and_waves.waves; points = List.rev !result_points; inputs }
    in
    let fpga_calculated_result = Utils.calculate_result_from_fpga result.points in
    let expected = Utils.expected inputs in
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

  let run_test
    ?(waves = false)
    ?(seed = 0)
    ?(timeout = 10_000)
    ?(verilator = false)
    num_inputs
    =
    run ~waves ~seed ~timeout ~verilator num_inputs ()
  ;;
end

let%expect_test "Test over small input size" =
  let module Config = struct
    let field_bits = 377
    let scalar_bits = 12
    let controller_log_stall_fifo_depth = 2
    let window_size_bits = 3
    let ram_read_latency = 1
  end
  in
  let module Test = Make (Config) in
  let _result = Test.run_test 8 in
  [%expect
    {|
    (Expecting (Top.num_result_points 28))
    (Got ("List.length (!result_points)" 28))
    PASS |}]
;;

let test_back_to_back () =
  let module Config = struct
    let field_bits = 377
    let scalar_bits = 13
    let controller_log_stall_fifo_depth = 2
    let window_size_bits = 3
    let ram_read_latency = 1
  end
  in
  let module Test = Make (Config) in
  let sim = Test.create ~waves:true ~verilator:false in
  let _result0 = Test.run ~sim ~seed:0 ~timeout:1000 ~verilator:false 8 () in
  let _result1 : Test.result =
    Test.run ~sim ~seed:1 ~timeout:1000 ~verilator:false 8 ()
  in
  let result2 : Test.result = Test.run ~sim ~seed:2 ~timeout:1000 ~verilator:false 8 () in
  Option.value_exn result2.waves
;;

let%expect_test "Test multiple back-back runs" =
  let _waves = test_back_to_back () in
  [%expect
    {|
    (Expecting (Top.num_result_points 36))
    (Got ("List.length (!result_points)" 32))
    ("ERROR: Result points did not match!"
     (expected
      ((x
        0x8faafc4b104fc10ab54361af7a11dba0481b5fa9e6e7d062d79895834154601b35f6c2116ceb31205233427eae85fd)
       (y
        0x186ba8034f66b54fd71d899e43e680edb0954b836374ec43cde13be83f8e534fc9457d1240f193ad2304e24b968114a)
       (infinity false)))
     (fpga_calculated_result
      ((x
        0x10fb5d94e461de9e293c0a0cb7b8de146609cf825119a0a45b91d75def920ab9a3e2d26a1fa98e7b3c27f4462e80258)
       (y
        0xd03efd7ed5fbe78aca9923698ac942b429c75b774e80c2b3bb57acc5076b083b7b8873583c2f1a9f81e8d3466c4169)
       (infinity false))))
    (Expecting (Top.num_result_points 36))
    (Got ("List.length (!result_points)" 0))
    ("ERROR: Result points did not match!"
     (expected
      ((x
        0x4b5fdeca9e4b1bd483fb1e9c97dc252a4b031a0c9ac695e59c149fb86b116df91255adc04c4d4f5d62bda52883419b)
       (y
        0xaa094ff9cdd775163f9e489960dfb54096a6b50aef9f5aaa5f5a795c4f63f77c2660b69505cfac2827e2236c64e4a0)
       (infinity false)))
     (fpga_calculated_result ((x 0x0) (y 0x1) (infinity true))))
    (Expecting (Top.num_result_points 36))
    (Got ("List.length (!result_points)" 0))
    ("ERROR: Result points did not match!"
     (expected
      ((x
        0x1a908f419767f0be956c9745107e0d4dd396a7fa1c13f1b2a4d0e3323bfdd9c158501f965bf2148b8fae577ae959f7e)
       (y
        0x47fcefdc4c413fc5efd889cfd7762f5e46b966be8e66a504a2c593d4b6cf7d82ba6e220642ed405bd15de79e1c64a3)
       (infinity false)))
     (fpga_calculated_result ((x 0x0) (y 0x1) (infinity true)))) |}]
;;
