open Core
open Hardcaml
open Hardcaml_waveterm
open Msm_pippenger

module Make (Config : Msm_pippenger.Config.S) = struct
  module Utils = Utils.Make (Config)
  module Config_utils = Config_utils.Make (Config)
  module Top = Top.Make (Config)
  module Kernel = Kernel_for_vitis.Make (Config)
  module I = Kernel.I
  module O = Kernel.O
  module I_rules = Display_rules.With_interface (Kernel.I)
  module O_rules = Display_rules.With_interface (Kernel.O)
  module Sim = Cyclesim.With_interface (I) (O)
  module Tb = Hardcaml_step_testbench.Make (I) (O)
  module Axi = Hardcaml_axi.Axi512.Stream
  module Tb_axi_send = Hardcaml_step_testbench.Make (Axi.Source) (Axi.Dest)
  module Tb_axi_recv = Hardcaml_step_testbench.Make (Axi.Dest) (Axi.Source)

  let precompute = Top.precompute

  let create_sim verilator () =
    let scope =
      Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ()
    in
    let create = Kernel.hierarchical ~build_mode:Simulation scope in
    if verilator
    then
      let module V = Hardcaml_verilator.With_interface (Kernel.I) (Kernel.O) in
      V.create
        ~clock_names:[ "clock"; "ap_clk" ]
        ~cache_dir:"/tmp/kernel/"
        ~verbose:true
        create
    else
      Sim.create
        ~config:{ Cyclesim.Config.trace_all with deduplicate_signals = false }
        create
  ;;

  let display_rules =
    List.concat [ I_rules.default (); O_rules.default (); [ Display_rule.default ] ]
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
    i.ap_rst_n := Bits.vdd;
    Cyclesim.cycle sim;
    if waves
    then (
      let waves, sim = Waveform.create sim in
      { waves = Some waves; sim })
    else { waves = None; sim }
  ;;

  let axi_bits = 512
  let aligned_to = 64
  let aligned_field_bits = Int.round_up Config.field_bits ~to_multiple_of:aligned_to
  let aligned_scalar_bits = Int.round_up Config.scalar_bits ~to_multiple_of:aligned_to
  let drop_t = false

  let num_clocks_per_output =
    Int.round_up ((if drop_t then 3 else 4) * aligned_field_bits) ~to_multiple_of:axi_bits
    / axi_bits
  ;;

  module Input_aligned = struct
    type 'a t =
      { x : 'a [@bits aligned_field_bits]
      ; y : 'a [@bits aligned_field_bits]
      ; t : 'a [@bits aligned_field_bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let aligned_point_bits =
    Input_aligned.(fold port_widths ~init:0 ~f:( + ))
    |> Int.round_up ~to_multiple_of:axi_bits
  ;;

  let run ?sim ?(waves = true) ~seed ~timeout ~verilator num_inputs () =
    let cycle_cnt = ref 0 in
    let sim_and_waves = Option.value sim ~default:(create ~verilator ~waves) in
    let sim = sim_and_waves.sim in
    let i, _o = Cyclesim.inputs sim, Cyclesim.outputs sim in
    let inputs =
      Utils.random_inputs
        ~precompute
        ~seed
        ~top_window_size:Config_utils.top_window_size
        num_inputs
    in
    print_s [%message "Expecting" (Top.num_result_points : int)];
    Cyclesim.cycle sim;
    i.fpga_to_host_dest.tready := Bits.vdd;
    let points =
      Array.map inputs ~f:(fun input ->
        let b =
          Input_aligned.Of_bits.pack
            { Input_aligned.x =
                Bits.uresize input.affine_point_with_t.x aligned_field_bits
            ; Input_aligned.y =
                Bits.uresize input.affine_point_with_t.y aligned_field_bits
            ; Input_aligned.t =
                Bits.uresize input.affine_point_with_t.t aligned_field_bits
            }
        in
        Bits.uresize b aligned_point_bits)
      |> Bits.of_array
      |> Bits.split_lsb ~part_width:axi_bits
    in
    let scalars =
      let b =
        Array.map inputs ~f:(fun input -> Bits.uresize input.scalar aligned_scalar_bits)
        |> Bits.of_array
      in
      let b = Bits.uresize b (Int.round_up (Bits.width b) ~to_multiple_of:axi_bits) in
      Bits.split_lsb ~part_width:axi_bits b
    in
    cycle_cnt := 0;
    let rec send_data data _ : unit Tb_axi_send.t =
      let open! Tb_axi_send.Let_syntax in
      if List.length data = 0
      then (
        let%bind _ = Tb_axi_send.cycle Tb_axi_send.input_zero in
        return ())
      else (
        let%bind o =
          Tb_axi_send.cycle
            { Tb_axi_send.input_hold with
              tvalid = Bits.vdd
            ; tdata = List.hd_exn data
            ; tlast = (if List.length data = 1 then Bits.vdd else Bits.gnd)
            }
        in
        let rec wait_for_tready (o : Tb_axi_send.O_data.t) =
          if Bits.is_vdd o.before_edge.tready
          then return ()
          else (
            let%bind o = Tb_axi_send.cycle Tb_axi_send.input_hold in
            wait_for_tready o)
        in
        let%bind () = wait_for_tready o in
        send_data (List.tl_exn data) o)
    in
    let cc = ref 0 in
    let open! Tb_axi_recv.Let_syntax in
    let rec recv_data data (output : Tb_axi_recv.O_data.t) : Bits.t list Tb_axi_recv.t =
      let result = Tb_axi_recv.O_data.after_edge output in
      if Bits.to_int result.tvalid <> 1
      then wait_for_next_cycle data
      else (
        let data = result.tdata :: data in
        if Bits.to_int result.tlast = 1
        then return (List.rev data)
        else wait_for_next_cycle data)
    and wait_for_next_cycle data =
      Int.incr cc;
      let%bind output = Tb_axi_recv.cycle { tready = Bits.vdd } in
      recv_data data output
    in
    let open! Tb.Let_syntax in
    let testbench _ =
      let%bind send_scalars_finished =
        Tb_axi_send.spawn_io
          ~inputs:(fun ~(parent : _ I.t) ~child ->
            { parent with
              host_scalars_to_fpga =
                Tb_axi_send.merge_inputs ~parent:parent.host_scalars_to_fpga ~child
            })
          ~outputs:(fun parent -> parent.O.host_scalars_to_fpga_dest)
          (send_data scalars)
      in
      let%bind send_points_finished =
        Tb_axi_send.spawn_io
          ~inputs:(fun ~(parent : _ I.t) ~child ->
            { parent with
              ddr_points_to_fpga =
                Tb_axi_send.merge_inputs ~parent:parent.ddr_points_to_fpga ~child
            })
          ~outputs:(fun parent -> parent.O.ddr_points_to_fpga_dest)
          (send_data points)
      in
      let%bind receive_finished =
        Tb_axi_recv.spawn_io
          ~inputs:(fun ~(parent : _ I.t) ~child ->
            { parent with
              fpga_to_host_dest =
                Tb_axi_recv.merge_inputs ~parent:parent.fpga_to_host_dest ~child
            })
          ~outputs:(fun parent -> parent.O.fpga_to_host)
          (recv_data [])
      in
      let%bind data = Tb.wait_for receive_finished in
      let%bind () = Tb.wait_for send_scalars_finished in
      let%bind () = Tb.wait_for send_points_finished in
      return data
    in
    let recv_data = Tb.run_with_timeout ~timeout ~simulator:sim ~testbench () in
    if Option.is_none recv_data then print_s [%message "Simulation timed out!"];
    let result_points = ref [] in
    let bucket = ref (Config_utils.num_buckets 0) in
    let window = ref 0 in
    Option.iter recv_data ~f:(fun recv_data ->
      let data = List.to_array recv_data in
      let points =
        Array.init
          (Array.length data / num_clocks_per_output)
          ~f:(fun point ->
            Bits.of_array
              (Array.init num_clocks_per_output ~f:(fun point_word ->
                 data.((point * num_clocks_per_output) + point_word))))
      in
      Array.iter points ~f:(fun point ->
        let to_z b = Bits.to_constant b |> Constant.to_z ~signedness:Unsigned in
        let extended : Utils.Twisted_edwards.extended =
          { x = to_z (Bits.select point (Config.field_bits - 1) 0)
          ; y =
              to_z
                (Bits.select
                   point
                   (aligned_field_bits + Config.field_bits - 1)
                   aligned_field_bits)
          ; t =
              (if drop_t
              then Z.zero
              else
                to_z
                  (Bits.select
                     point
                     ((3 * aligned_field_bits) + Config.field_bits - 1)
                     (aligned_field_bits * 3)))
          ; z =
              to_z
                (Bits.select
                   point
                   ((2 * aligned_field_bits) + Config.field_bits - 1)
                   (aligned_field_bits * 2))
          }
        in
        let affine =
          Utils.twisted_edwards_extended_to_affine ~precompute ~has_t:true extended
        in
        result_points
          := { Utils.point = affine; bucket = !bucket; window = !window }
             :: !result_points;
        bucket
          := if !bucket = 1
             then (
               Int.incr window;
               Config_utils.num_buckets !window)
             else !bucket - 1));
    print_s [%message "Got" (List.length !result_points : int)];
    [%test_result: int] ~expect:Top.num_result_points (List.length !result_points);
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
    let num_windows = 4
    let window_ram_partition_settings = None
  end
  in
  let module Test = Make (Config) in
  let _result = Test.run_test 8 in
  [%expect
    {|
    (Expecting (Top.num_result_points 19))
    (Got ("List.length (!result_points)" 19))
    PASS |}]
;;

let test_back_to_back () =
  let module Config = struct
    let field_bits = 377
    let scalar_bits = 13
    let controller_log_stall_fifo_depth = 2
    let window_ram_partition_settings = None
    let num_windows = 4
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
    (Expecting (Top.num_result_points 23))
    (Got ("List.length (!result_points)" 23))
    PASS
    (Expecting (Top.num_result_points 23))
    (Got ("List.length (!result_points)" 23))
    PASS
    (Expecting (Top.num_result_points 23))
    (Got ("List.length (!result_points)" 23))
    PASS |}]
;;
