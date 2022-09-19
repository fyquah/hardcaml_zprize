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
        printf "i = %d, bucket = %d\n" i bucket;
        windows.(i).(bucket)
          <- Utils.Twisted_edwards.add_unified
               (force Twisted_edwards_model_lib.Bls12_377_params.twisted_edwards)
               windows.(i).(bucket)
               p;
        lo := !lo + window_size_bits));
    Array.map windows ~f:(fun window ->
      Array.map window ~f:(fun p -> Utils.twisted_edwards_extended_to_affine p))
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
    let expected_bucket_outputs = expected_bucket_outputs inputs in
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
        i.host_to_fpga.tvalid := Bits.random ~width:1;
        if beat = num_clocks_per_input - 1 && idx = num_inputs - 1
        then i.host_to_fpga.tlast := Bits.vdd;
        if Bits.is_gnd !(i.host_to_fpga.tvalid) then Cyclesim.cycle sim;
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
          let extended : Utils.Twisted_edwards.extended =
            { x = to_z result_point.x
            ; y = to_z result_point.y
            ; t = to_z result_point.t
            ; z = to_z result_point.z
            }
          in
          printf "bucket = %d, window = %d core_index = %d\n" !bucket !window !core_index;
          let affine = Utils.twisted_edwards_extended_to_affine extended in
          let expected = expected_bucket_outputs.(!window).(!bucket) in
          if not ([%equal: Utils.Weierstrass.affine option] affine expected)
          then printf "Doesnt match expected!\n";
          result_points
            := { Utils.point = affine; bucket = !bucket; window = !window }
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
      Cyclesim.cycle sim
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
      ; scalar_bits_by_core = [| 5; 5; 5 |]
      ; controller_log_stall_fifo_depth = 2
      ; window_size_bits = 2
      ; ram_read_latency = 1
      }
    ;;
  end
  in
  let module Test = Make (Config) in
  let _result = Test.run_test 4 in
  [%expect
    {|
    (all_window_size_bits (2 3 2 3 2 3))
    i = 0, bucket = 2
    i = 1, bucket = 7
    i = 2, bucket = 2
    i = 3, bucket = 4
    i = 4, bucket = 1
    i = 5, bucket = 2
    i = 0, bucket = 0
    i = 1, bucket = 2
    i = 2, bucket = 1
    i = 3, bucket = 6
    i = 4, bucket = 2
    i = 5, bucket = 6
    i = 0, bucket = 3
    i = 1, bucket = 2
    i = 2, bucket = 0
    i = 3, bucket = 3
    i = 4, bucket = 0
    i = 5, bucket = 7
    i = 0, bucket = 2
    i = 1, bucket = 4
    i = 2, bucket = 0
    i = 3, bucket = 3
    i = 4, bucket = 1
    i = 5, bucket = 2
    (Expecting (num_result_points 30))
    bucket = 3, window = 0 core_index = 0
    bucket = 2, window = 0 core_index = 0
    bucket = 1, window = 0 core_index = 0
    bucket = 7, window = 1 core_index = 0
    bucket = 6, window = 1 core_index = 0
    bucket = 5, window = 1 core_index = 0
    bucket = 4, window = 1 core_index = 0
    bucket = 3, window = 1 core_index = 0
    bucket = 2, window = 1 core_index = 0
    bucket = 1, window = 1 core_index = 0
    bucket = 3, window = 2 core_index = 1
    bucket = 2, window = 2 core_index = 1
    ("Check for t did not pass!"
     (x
      0x98f6380f51d7c13d772419496e7f39d20867f16ff93c4c771fd04cedd667bf060ed502eb9ef632c216c4c27ea58f9f)
     (y
      0x16602b9cafe46f078e2100b404378edf8b91677ea0bd2ee05490cd8d10f17e986afae15e30ccdd4c89769d1fb18ef70)
     (z 0x4)
     (t
      0x6df221648756658d9bc0ca4052db4315137a501789396366c4157895e430fce24b303ea631bcb423a13364370cf2d9))
    Doesnt match expected!
    bucket = 1, window = 2 core_index = 1
    ("Check for t did not pass!"
     (x
      0x14d2f314073ca1ceabcd1cbf6f5e38cbb2b4d48637d881761ffd84b0d2e175a949669c8a74ae27411ff865757beb829)
     (y
      0x11acbe6e2a39fee4e7e05a84f5917f31d00b0ceb4d290578f63cc3ef95b75787baed0c2572a781f2891b3ca0cff9660)
     (z 0x4)
     (t
      0x2eff1656bdbebeb7e207ac18413d1dbb6f3051c242f4cc0f5522a1217efef5d1cf7ec0db950a6079da498a9642fc72))
    Doesnt match expected!
    bucket = 7, window = 3 core_index = 1
    bucket = 6, window = 3 core_index = 1
    bucket = 5, window = 3 core_index = 1
    bucket = 4, window = 3 core_index = 1
    bucket = 3, window = 3 core_index = 1
    ("Check for t did not pass!"
     (x
      0x97882940458437ee67bb4447f1d6dbab4071abebcd844f8e9d1f59b4fff879101819d9ae664a2f322894c03f5ae636)
     (y
      0x5d68ff0388cee8578b18285d566cc699964788fb0d4e49e4c07c5b8e05ee430a7955739c3f952ca8c1429138958601)
     (z
      0x53be1f86e7b7e59b2dc8abca7409783b1b741136f79281ca8652df0d3ed76bd8d125b2e3149837fe199b424e2c2681)
     (t
      0x67a034d046ea1d573ce4de893eab9a9bc11d09240529a8719b6e4b94e3dc62b538456391f9122be0706453aab6580d))
    Doesnt match expected!
    bucket = 2, window = 3 core_index = 1
    bucket = 1, window = 3 core_index = 1
    bucket = 3, window = 4 core_index = 2
    bucket = 2, window = 4 core_index = 2
    bucket = 1, window = 4 core_index = 2
    bucket = 7, window = 5 core_index = 2
    bucket = 6, window = 5 core_index = 2
    bucket = 5, window = 5 core_index = 2
    bucket = 4, window = 5 core_index = 2
    bucket = 3, window = 5 core_index = 2
    bucket = 2, window = 5 core_index = 2
    bucket = 1, window = 5 core_index = 2
    (Got ("List.length (!result_points)" 30))
    ("ERROR: Result points did not match!"
     (expected
      ((x
        0x1152c62d42035ce0601165c58d2c34674d5a5b330ad1891a4039088e74e20290a351384ce0542854ce9455e32d06a9b)
       (y
        0x156cc9ed9863f6dccc3e05e1aa3a1bcbfe9449103ad7d655d974c731d959d59dcbdc51d47c669544b9e47f9fcd8e70a)
       (infinity false)))
     (fpga_calculated_result
      ((x
        0x1584cc086fcdcacc8b5f0407041a1da09cad7ff209e9ae156a8bdb4f55e72363fc08623369fad41bb33514733cee05b)
       (y
        0x721c9b91f3ab595c14280bab0e738d80ac6ea57b49d72aadab461fe022f456c86e100e838e5c223a90db811749ce76)
       (infinity false)))) |}]
;;
