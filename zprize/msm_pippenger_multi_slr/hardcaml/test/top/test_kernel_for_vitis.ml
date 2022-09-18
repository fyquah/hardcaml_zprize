open Core
open Hardcaml
open Hardcaml_waveterm
open Msm_pippenger_multi_slr

module Make (Config : Msm_pippenger_multi_slr.Config.S) = struct
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

  (* CR-someday fyquah: Largely a duplicate of the contents of
   * test_kernel_for_single_instance.ml ..
   *)
  let run ?sim ?(waves = true) ~seed ~timeout ~verilator num_inputs () =
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
          let affine = Utils.twisted_edwards_extended_to_affine extended in
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
end
