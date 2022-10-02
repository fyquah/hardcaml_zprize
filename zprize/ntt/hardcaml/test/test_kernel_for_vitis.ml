open! Core
open! Hardcaml
open! Hardcaml_waveterm
module Gf = Hardcaml_ntt.Gf

module Make (Config : Zprize_ntt.Top_config.S) = struct
  open Config
  module Kernel = Zprize_ntt.For_vitis.Make (Config)
  module Reference_model = Hardcaml_ntt.Reference_model.Make (Gf.Z)
  module Test_top = Test_top.Make (Config)
  module Sim = Cyclesim.With_interface (Kernel.I) (Kernel.O)
  module VSim = Hardcaml_verilator.With_interface (Kernel.I) (Kernel.O)

  (* Derived parameters *)
  let n = 1 lsl logn
  let num_cores = 1 lsl logcores
  let num_blocks = 1 lsl logblocks
  let log_passes = logn - logcores
  let num_passes = 1 lsl log_passes
  let () = assert (1 lsl (logn + logn) = n * num_cores * num_passes)

  (* Common functions *)
  let random_input_coef_matrix = Test_top.random_input_coef_matrix
  let print_matrix = Test_top.print_matrix
  let copy_matrix = Test_top.copy_matrix
  let convert_to_first_pass_input = Test_top.convert_to_first_pass_input
  let get_first_pass_results = Test_top.get_first_pass_results
  let check_first_pass_output = Test_top.check_first_pass_output
  let random_bool ~p_true = Float.(Random.float 1.0 < p_true)
  let get_second_pass_results = get_first_pass_results

  let create_sim ~verilator waves =
    let sim =
      if verilator
      then (
        let cache_dir = Sys.getenv_exn "HOME" ^/ ".hardcaml-verilator-cache" in
        VSim.create
          ~cache_dir
          ~verbose:true
          ~clock_names:[ "ap_clk" ]
          (Kernel.create
             ~build_mode:Simulation
             (Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ())))
      else
        Sim.create
          ~config:Cyclesim.Config.trace_all
          (Kernel.create
             ~build_mode:Simulation
             (Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ()))
    in
    let inputs = Cyclesim.inputs sim in
    let outputs = Cyclesim.outputs sim in
    let waves, sim =
      if waves
      then (
        let waves, sim = Waveform.create sim in
        Some waves, sim)
      else None, sim
    in
    sim, waves, inputs, outputs
  ;;

  let start_sim (inputs : _ Kernel.I.t) cycle =
    inputs.ap_rst_n := Bits.gnd;
    cycle ();
    inputs.ap_rst_n := Bits.vdd;
    inputs.compute_to_controller_dest.tready := Bits.vdd;
    cycle ();
    cycle ()
  ;;

  (* Perform the reference intt by using a standard full size single pass,
     rather than the 4step algorithm the hw uses. *)
  let reference_intt coefs =
    let coefs = Array.concat (Array.to_list (Array.copy coefs)) in
    Reference_model.inverse_dit coefs;
    Array.init n ~f:(fun row -> Array.init n ~f:(fun col -> coefs.((row * n) + col)))
  ;;

  let convert_from_second_pass_output output =
    let r = Array.init n ~f:(fun _ -> Array.init n ~f:(Fn.const Gf.Z.zero)) in
    let pos = ref 0 in
    for col1 = 0 to (n lsr (logcores + logblocks)) - 1 do
      for row = 0 to n - 1 do
        for col2 = 0 to (1 lsl (logcores + logblocks)) - 1 do
          r.(row).((col1 * num_cores * num_blocks) + col2) <- output.(!pos);
          Int.incr pos
        done
      done
    done;
    r
  ;;

  let convert_first_pass_output_to_stream coefs =
    let out = Array.init (Array.length coefs) ~f:(Fn.const Gf.Z.zero) in
    let outpos = ref 0 in
    let step1 = num_cores * num_cores * num_blocks in
    let step2 = n * num_cores * num_blocks in
    for block_col = 0 to (n lsr logcores) - 1 do
      for block_row = 0 to (n lsr (logcores + logblocks)) - 1 do
        let pos = (block_col * step1) + (block_row * step2) in
        for word = 0 to (num_cores * num_blocks) - 1 do
          for j = 0 to num_cores - 1 do
            out.(!outpos) <- coefs.(pos + (word * num_cores) + j);
            Int.incr outpos
          done
        done
      done
    done;
    out
  ;;

  (* Check the final results using a standard full size ntt. *)
  let check_second_pass_output ~verbose input_coefs hw_results =
    let sw_results = reference_intt input_coefs in
    let hw_results = convert_from_second_pass_output hw_results in
    if verbose
    then
      List.iter
        [ "inputs", input_coefs; "sw", sw_results; "hw", hw_results ]
        ~f:(fun (n, m) ->
          printf "\n%s\n\n" n;
          print_matrix m);
    if [%equal: Gf.Z.t array array] hw_results sw_results
    then print_s [%message "Hardware and software reference results match!"]
    else raise_s [%message "ERROR: Hardware and software results do not match :("]
  ;;

  let sync_to_last_result ~cycle ~num_results =
    while !num_results <> n * n / num_cores do
      cycle ()
    done
  ;;

  let load_stream
    ~wiggle_prob
    (controller_to_compute : _ Kernel.Axi_stream.Source.t)
    (controller_to_compute_dest : _ Kernel.Axi_stream.Dest.t)
    cycle
    coefs
    =
    for i = 0 to (Array.length coefs / num_cores) - 1 do
      controller_to_compute.tvalid := Bits.gnd;
      while not (random_bool ~p_true:wiggle_prob) do
        cycle ()
      done;
      controller_to_compute.tvalid := Bits.vdd;
      controller_to_compute.tdata
        := List.init num_cores ~f:(fun j ->
             Gf.Bits.to_bits (Gf.Bits.of_z (Gf.Z.to_z coefs.((i * num_cores) + j))))
           |> Bits.concat_lsb;
      while Bits.is_gnd !(controller_to_compute_dest.tready) do
        cycle ()
      done;
      cycle ();
      controller_to_compute.tvalid := Bits.gnd
    done
  ;;

  let run_first_pass
    ~wiggle_prob
    ~(inputs : _ Kernel.I.t)
    ~(outputs : _ Kernel.O.t)
    ~coefs
    ~cycle
    ~num_results
    ~results
    =
    num_results := 0;
    results := [];
    let coefs = convert_to_first_pass_input coefs in
    let controller_to_compute = inputs.controller_to_compute_phase_1 in
    let controller_to_compute_dest = outputs.controller_to_compute_phase_1_dest in
    (* cheat - force the core to [start] *)
    controller_to_compute.tvalid := Bits.vdd;
    cycle ();
    (* wait for tready *)
    assert (Bits.to_bool !(controller_to_compute_dest.tready));
    load_stream ~wiggle_prob controller_to_compute controller_to_compute_dest cycle coefs;
    sync_to_last_result ~cycle ~num_results;
    get_first_pass_results !results
  ;;

  let run_second_pass
    ~wiggle_prob
    ~(inputs : _ Kernel.I.t)
    ~(outputs : _ Kernel.O.t)
    ~coefs
    ~(cycle : ?n:int -> unit -> unit)
    ~num_results
    ~results
    =
    num_results := 0;
    results := [];
    let controller_to_compute = inputs.controller_to_compute_phase_2 in
    let controller_to_compute_dest = outputs.controller_to_compute_phase_2_dest in
    load_stream ~wiggle_prob controller_to_compute controller_to_compute_dest cycle coefs;
    sync_to_last_result ~cycle ~num_results;
    get_second_pass_results !results
  ;;

  let run
    ?(verbose = false)
    ?(waves = false)
    ?(verilator = false)
    ?(wiggle_prob = 1.)
    (input_coefs : Z.t array array)
    =
    let sim, waves, inputs, outputs = create_sim ~verilator waves in
    let input_coefs = Array.map input_coefs ~f:(Array.map ~f:Gf.Z.of_z) in
    let results = ref [] in
    let num_results = ref 0 in
    let x = ref 0 in
    let cycle ?(n = 1) () =
      assert (n > 0);
      Int.incr x;
      for _ = 1 to n do
        let tready = random_bool ~p_true:wiggle_prob in
        if Bits.to_bool !(outputs.compute_to_controller.tvalid) && tready
        then (
          results := !(outputs.compute_to_controller.tdata) :: !results;
          Int.incr num_results);
        inputs.compute_to_controller_dest.tready := Bits.of_bool tready;
        Cyclesim.cycle sim
      done;
      if false && !x > 10_000 then raise_s [%message "Running too long"]
    in
    start_sim inputs cycle;
    (try
       let pass1 =
         run_first_pass
           ~wiggle_prob
           ~inputs
           ~outputs
           ~coefs:input_coefs
           ~cycle
           ~num_results
           ~results
       in
       check_first_pass_output ~verbose input_coefs pass1;
       let pass2 =
         run_second_pass
           ~wiggle_prob
           ~inputs
           ~outputs
           ~coefs:(convert_first_pass_output_to_stream pass1)
           ~cycle
           ~num_results
           ~results
       in
       cycle ~n:4 ();
       check_second_pass_output ~verbose input_coefs pass2
     with
     | e -> print_s [%message "RAISED :(" (e : exn)]);
    waves
  ;;
end

let%expect_test "vitis kernel test" =
  let module Config = struct
    let logn = 5
    let logcores = 3
    let logblocks = 0
    let support_4step_twiddle = true
    let memory_layout = Zprize_ntt.Memory_layout.Optimised_layout_single_port
  end
  in
  let module Test = Make (Config) in
  let input_coefs = Test.random_input_coef_matrix () in
  ignore
    (Test.run ~verilator:false ~verbose:false ~waves:false input_coefs
      : Waveform.t option);
  [%expect
    {|
    ("Hardware and software reference results match!" (pass first))
    "Hardware and software reference results match!" |}]
;;

let%expect_test "2 blocks" =
  let module Config = struct
    let logn = 5
    let logcores = 3
    let logblocks = 1
    let support_4step_twiddle = true
    let memory_layout = Zprize_ntt.Memory_layout.Optimised_layout_single_port
  end
  in
  let module Test = Make (Config) in
  let input_coefs = Test.random_input_coef_matrix () in
  ignore
    (Test.run ~verilator:false ~verbose:false ~waves:false input_coefs
      : Waveform.t option);
  [%expect
    {|
    ("Hardware and software reference results match!" (pass first))
    "Hardware and software reference results match!" |}]
;;

let%expect_test "normal layout" =
  let module Config = struct
    let logn = 5
    let logcores = 3
    let logblocks = 1
    let support_4step_twiddle = true
    let memory_layout = Zprize_ntt.Memory_layout.Normal_layout_single_port
  end
  in
  let module Test = Make (Config) in
  let input_coefs = Test.random_input_coef_matrix () in
  ignore
    (Test.run ~verilator:false ~verbose:false ~waves:false input_coefs
      : Waveform.t option);
  [%expect
    {|
    ("Hardware and software reference results match!" (pass first))
    "Hardware and software reference results match!" |}]
;;
