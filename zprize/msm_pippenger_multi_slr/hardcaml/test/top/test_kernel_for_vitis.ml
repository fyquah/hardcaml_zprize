open Core
open Hardcaml
open Hardcaml_waveterm
open Msm_pippenger_multi_slr

let () = Hardcaml.Caller_id.set_mode Full_trace

module Make (Config : Msm_pippenger_multi_slr.Config.S) = struct
  let config = Config.t

  module Utils = Utils.Make (Config)
  module Kernel = Kernel_for_vitis.Make (Config)
  module I = Kernel.I
  module O = Kernel.O
  module Sim = Cyclesim.With_interface (I) (O)

  type sim_and_waves =
    { sim : Sim.t
    ; waves : Waveform.t option
    }

  type result =
    { waves : Waveform.t option
    ; points : Utils.window_bucket_point list
    ; inputs : Bits.t Utils.Msm_input.t array
    }

  let num_result_points = Msm_pippenger_multi_slr.Config.num_result_points Config.t
  let scalar_bits = Msm_pippenger_multi_slr.Config.scalar_bits Config.t

  let num_clocks_per_input =
    Int.round_up (scalar_bits + (3 * Config.t.field_bits)) ~to_multiple_of:512 / 512
  ;;

  let num_clocks_per_output =
    Int.round_up (4 * Config.t.field_bits) ~to_multiple_of:512 / 512
  ;;

  let num_cores = Array.length Config.t.scalar_bits_by_core

  let create_sim ~verilator () =
    let scope =
      Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ()
    in
    let create = Kernel.hierarchical ~build_mode:Simulation scope in
    if verilator
    then
      let module V = Hardcaml_verilator.With_interface (Kernel.I) (Kernel.O) in
      V.create ~clock_names:[ "ap_clk" ] ~cache_dir:"/tmp/kernel/" ~verbose:true create
    else Sim.create ~config:Cyclesim.Config.trace_all create
  ;;

  let create ~verilator ~waves =
    let sim = create_sim ~verilator () in
    let i = Cyclesim.inputs sim in
    i.ap_rst_n := Bits.gnd;
    Cyclesim.cycle sim;
    Cyclesim.cycle sim;
    i.ap_rst_n := Bits.vdd;
    for _ = 1 to 3 do
      Cyclesim.cycle sim
    done;
    if waves
    then (
      let waves, sim = Waveform.create sim in
      { waves = Some waves; sim })
    else { waves = None; sim }
  ;;

  let expected_bucket_outputs (input : _ Utils.Msm_input.t array) =
    let to_z b = Bits.to_constant b |> Constant.to_z ~signedness:Unsigned in
    let all_window_size_bits =
      Msm_pippenger_multi_slr.Config.all_window_size_bits config |> Array.of_list
    in
    print_s [%message (all_window_size_bits : int array)];
    let windows =
      Array.map all_window_size_bits ~f:(fun window_num_bits ->
        Array.init (1 lsl window_num_bits) ~f:(fun _ ->
          Utils.Twisted_edwards.(affine_identity |> affine_to_extended ~z:Z.one)))
    in
    Array.iter input ~f:(fun input ->
      let p : Utils.Twisted_edwards.affine_with_t =
        { x = to_z input.affine_point_with_t.x
        ; y = to_z input.affine_point_with_t.y
        ; t = to_z input.affine_point_with_t.t
        }
      in
      let lo = ref 0 in
      Array.iteri all_window_size_bits ~f:(fun i window_size_bits ->
        let bucket =
          Bits.to_int (Bits.select input.scalar (!lo + window_size_bits - 1) !lo)
        in
        windows.(i).(bucket)
          <- Utils.Twisted_edwards.add_unified
               (force Twisted_edwards_model_lib.Bls12_377_params.twisted_edwards)
               windows.(i).(bucket)
               p;
        lo := !lo + window_size_bits));
    windows
  ;;

  let debug = true

  (* CR-someday fyquah: Largely a duplicate of the contents of
   * test_kernel_for_single_instance.ml ..
   *)
  let run ?sim ?(waves = true) ~seed ~timeout ~verilator num_inputs () =
    let cycle_cnt = ref 0 in
    let sim_and_waves = Option.value sim ~default:(create ~verilator ~waves) in
    let sim = sim_and_waves.sim in
    let i, o = Cyclesim.inputs sim, Cyclesim.outputs sim in
    if debug
    then
      at_exit (fun () ->
        Option.iter sim_and_waves.waves ~f:(fun waves ->
          Waveform.Serialize.marshall waves "a.hardcamlwaveform.Z"));
    let inputs = Utils.random_inputs ~seed num_inputs in
    (*
    Array.iter inputs ~f:(fun input ->
      let affine_point_with_t =
        Utils.Affine_point_with_t.map
          ~f:(Bits.to_z ~signedness:Unsigned)
          input.affine_point_with_t
      in
      let sexp_of_z z = Sexp.Atom (Z.format "x" z) in
      print_s ([%sexp_of: z Utils.Affine_point_with_t.t] affine_point_with_t));
       *)
    let expected_bucket_outputs = expected_bucket_outputs inputs in
    let cycle () = Cyclesim.cycle sim in
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
        i.host_to_fpga.tvalid := Bits.random ~width:1;
        if beat = num_clocks_per_input - 1 && idx = num_inputs - 1
        then i.host_to_fpga.tlast := Bits.vdd;
        if Bits.is_gnd !(i.host_to_fpga.tvalid) then cycle ();
        i.host_to_fpga.tvalid := Bits.vdd;
        cycle_cnt := 0;
        while Bits.is_gnd !(o.host_to_fpga_dest.tready) && !cycle_cnt < timeout do
          Int.incr cycle_cnt;
          cycle ()
        done;
        cycle ();
        to_send := Bits.(srl !to_send 512)
      done;
      i.host_to_fpga.tvalid := Bits.gnd;
      for _ = 0 to Random.int 5 do
        cycle ()
      done
    done;
    i.host_to_fpga.tlast := Bits.gnd;
    i.host_to_fpga.tvalid := Bits.gnd;
    cycle ();
    cycle_cnt := 0;
    while Bits.is_gnd !(o.fpga_to_host.tvalid) && !cycle_cnt < timeout do
      Int.incr cycle_cnt;
      cycle ()
    done;
    cycle_cnt := 0;
    let word = ref 0 in
    let output_buffer_bits = num_clocks_per_output * 512 in
    let output_buffer = ref (Bits.zero output_buffer_bits) in
    let result_points = ref [] in
    let core_index = ref 0 in
    let window = ref 0 in
    let bucket = ref ((1 lsl Config.t.window_size_bits) - 1) in
    let is_last = ref false in
    print_s [%message "Expecting" (num_result_points : int)];
    while (not !is_last) && !cycle_cnt < timeout do
      i.fpga_to_host_dest.tready := Bits.random ~width:1;
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
          let obtained_extended : Utils.Twisted_edwards.extended =
            { x = to_z result_point.x
            ; y = to_z result_point.y
            ; t = to_z result_point.t
            ; z = to_z result_point.z
            }
          in
          let obtained = Utils.twisted_edwards_extended_to_affine obtained_extended in
          let expected_extended = expected_bucket_outputs.(!window).(!bucket) in
          let expected = Utils.twisted_edwards_extended_to_affine expected_extended in
          if not ([%equal: Utils.Weierstrass.affine option] obtained expected)
          then
            print_s
              [%message
                "bucket result Doesnt match expected!"
                  (!core_index : int)
                  (!window : int)
                  (!bucket : int)
                  (obtained_extended : Utils.Twisted_edwards.extended)
                  (expected_extended : Utils.Twisted_edwards.extended)
                  (expected : Utils.Weierstrass.affine option)
                  (obtained : Utils.Weierstrass.affine option)];
          result_points
            := { Utils.point = obtained; bucket = !bucket; window = !window }
               :: !result_points;
          (* Increment bucket (or window, or core_index) *)
          With_return.with_return (fun { return } ->
            assert (!bucket >= 1);
            (* Very simple case *)
            if !bucket <> 1
            then (
              bucket := !bucket - 1;
              return ());
            let is_last_window_for_this_core =
              !window
              = Msm_pippenger_multi_slr.Config.last_window_for_core
                  Config.t
                  ~core_index:!core_index
            in
            if is_last_window_for_this_core
            then core_index := (!core_index + 1) mod num_cores;
            window := !window + 1;
            let next_window_size_bits =
              if !window
                 = Msm_pippenger_multi_slr.Config.last_window_for_core
                     Config.t
                     ~core_index:!core_index
              then
                Msm_pippenger_multi_slr.Config.last_window_size_bits_for_slr
                  Config.t
                  !core_index
              else
                Msm_pippenger_multi_slr.Config.window_size_bits_for_slr
                  Config.t
                  !core_index
            in
            bucket := (1 lsl next_window_size_bits) - 1)));
      cycle ()
    done;
    print_s [%message "Got" (List.length !result_points : int)];
    if !cycle_cnt = timeout then print_s [%message "Simulation timed out!"];
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
    if debug
    then
      Option.iter sim_and_waves.waves ~f:(fun waves ->
        Waveform.Serialize.marshall waves "a.hardcamlwaveform");
    result
  ;;

  let run_test
    ?(waves = debug)
    ?(seed = 0)
    ?(timeout = 20_000)
    ?(verilator = false)
    num_inputs
    =
    run ~waves ~seed ~timeout ~verilator num_inputs ()
  ;;
end

let%expect_test "Test over small input size" =
  let module Config = struct
    let t =
      { Msm_pippenger_multi_slr.Config.field_bits = 377
      ; scalar_bits_by_core = [| 7; 7; 7 |]
      ; controller_log_stall_fifo_depth = 2
      ; window_size_bits = 2
      ; ram_read_latency = 1
      }
    ;;
  end
  in
  let module Test = Make (Config) in
  let _result = Test.run_test 3 in
  [%expect
    {|
    (all_window_size_bits (2 2 3 2 2 3 2 2 3))
    (Expecting (num_result_points 39))
    (Got ("List.length (!result_points)" 39))
    PASS |}]
;;
