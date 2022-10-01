open Core
open Hardcaml
open Hardcaml_waveterm
module Gf = Hardcaml_ntt.Gf

module Make (Config : Zprize_ntt.Top_config.S) = struct
  module Reference_model = Hardcaml_ntt.Reference_model.Make (Gf.Z)
  module Top = Zprize_ntt.Top.Make (Config)
  module Sim = Cyclesim.With_interface (Top.I) (Top.O)

  let logn = Config.logn
  let n = 1 lsl logn
  let logcores = Config.logcores
  let logblocks = Config.logblocks
  let num_cores = 1 lsl logcores
  let log_passes = logn - logcores
  let num_passes = 1 lsl log_passes
  let num_blocks = 1 lsl logblocks
  let () = assert (1 lsl (logn + logn) = n * num_cores * num_passes)

  let random_input_coef_matrix () =
    Array.init (1 lsl logn) ~f:(fun _ ->
      Array.init (1 lsl logn) ~f:(fun _ -> Gf.Z.random () |> Gf.Z.to_z))
  ;;

  let twiddle m =
    Reference_model.apply_twiddles Reference_model.inverse_roots.(logn + logn) m
  ;;

  let print_matrix c =
    let hex z =
      Gf.Z.to_z z
      |> Gf.Bits.of_z
      |> Gf.Bits.to_bits
      |> Bits.to_constant
      |> Constant.to_hex_string ~signedness:Unsigned
    in
    Array.iteri c ~f:(fun row c ->
      printf "%.3i| " row;
      Array.iteri c ~f:(fun col c ->
        if col <> 0 && col % 8 = 0 then printf "\n   | ";
        printf "%16s " (hex c));
      printf "\n")
  ;;

  let copy_matrix c = Array.map c ~f:Array.copy
  let transpose = Reference_model.transpose

  let create_sim waves =
    let sim =
      Sim.create
        ~config:Cyclesim.Config.trace_all
        (Top.create
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

  let start_sim (inputs : _ Top.I.t) cycle =
    inputs.clear := Bits.vdd;
    cycle ();
    inputs.clear := Bits.gnd;
    inputs.data_out_dest.tready := Bits.vdd;
    inputs.start := Bits.vdd;
    cycle ();
    inputs.start := Bits.gnd;
    cycle ()
  ;;

  let convert_to_first_pass_input_optimised_layout inputs =
    let t = Array.init (n * n) ~f:(Fn.const Gf.Z.zero) in
    let pos = ref 0 in
    for block_col = 0 to (n lsr logcores) - 1 do
      for row = 0 to n - 1 do
        for i = 0 to num_cores - 1 do
          t.(!pos) <- inputs.(row).((block_col * num_cores) + i);
          Int.incr pos
        done
      done
    done;
    assert (!pos = n * n);
    t
  ;;

  let convert_to_first_pass_input_normal_layout inputs =
    let t = Array.init (n * n) ~f:(Fn.const Gf.Z.zero) in
    let pos = ref 0 in
    for block_col = 0 to (n lsr (logcores + logblocks)) - 1 do
      for row = 0 to n - 1 do
        for i = 0 to (num_cores * num_blocks) - 1 do
          t.(!pos) <- inputs.(row).((block_col * (num_cores * num_blocks)) + i);
          Int.incr pos
        done
      done
    done;
    assert (!pos = n * n);
    t
  ;;

  let convert_to_first_pass_input =
    match (Config.memory_layout : Zprize_ntt.Memory_layout.t) with
    | Normal_layout_single_port -> convert_to_first_pass_input_normal_layout
    | Optimised_layout_single_port -> convert_to_first_pass_input_optimised_layout
    | Normal_layout_multi_port -> raise_s [%message "Not implemented"]
  ;;

  let convert_first_pass_output_to_matrix output =
    let convert_packed_block t output ~pos ~row ~col =
      for r = 0 to num_cores - 1 do
        for c = 0 to num_cores - 1 do
          t.(row + r).(col + c) <- output.(pos + c + (r * num_cores))
        done
      done
    in
    let convert_packed_blocks t output ~pos ~row ~col =
      for block = 0 to num_blocks - 1 do
        convert_packed_block
          t
          output
          ~pos:(pos + (block * num_cores * num_cores))
          ~row
          ~col:(col + (block * num_cores))
      done
    in
    (* Uncomment for debugging. *)
    (* print_matrix [| output |]; *)
    let t = Array.init n ~f:(fun _ -> Array.init n ~f:(Fn.const Gf.Z.zero)) in
    for col = 0 to (1 lsl (logn - logcores - logblocks)) - 1 do
      for row = 0 to (1 lsl (logn - logcores)) - 1 do
        convert_packed_blocks
          t
          output
          ~pos:
            ((col * n * num_cores * num_blocks)
            + (row * num_cores * num_cores * num_blocks))
          ~row:(row * num_cores)
          ~col:(col * num_cores * num_blocks)
      done
    done;
    t
  ;;

  let get_first_pass_results (results : Bits.t list) =
    let results =
      List.map (List.rev results) ~f:(fun x -> Bits.split_lsb ~part_width:64 x)
      |> List.concat
    in
    let results =
      Array.of_list
        (List.map results ~f:(fun x -> Gf.Bits.of_bits x |> Gf.Bits.to_z |> Gf.Z.of_z))
    in
    results
  ;;

  let print_results ~verbose ~input_coefs ~sw_results ~hw_results ~pass =
    if verbose
    then
      List.iter
        [ "inputs", input_coefs; "sw", sw_results; "hw", hw_results ]
        ~f:(fun (n, m) ->
          printf "\n%s\n\n" n;
          print_matrix m);
    if [%equal: Gf.Z.t array array] hw_results sw_results
    then
      print_s [%message "Hardware and software reference results match!" (pass : string)]
    else
      raise_s
        [%message "ERROR: Hardware and software results do not match :(" (pass : string)]
  ;;

  let check_first_pass_output ~verbose input_coefs hw_results =
    let hw_results = convert_first_pass_output_to_matrix hw_results in
    (* column transform, followed by twiddles *)
    let sw_results = transpose (copy_matrix input_coefs) in
    Array.iter sw_results ~f:Reference_model.inverse_dit;
    twiddle sw_results;
    let sw_results = transpose sw_results in
    print_results ~verbose ~input_coefs ~sw_results ~hw_results ~pass:"first"
  ;;

  let create_cycle sim (outputs : _ Top.O.t) =
    let results = ref [] in
    let cycles = ref 0 in
    let rec cycle ?(n = 1) () =
      assert (n > 0);
      if Bits.to_bool !(outputs.data_out.tvalid)
      then results := !(outputs.data_out.tdata) :: !results;
      Cyclesim.cycle sim;
      Int.incr cycles;
      if n <> 1 then cycle ~n:(n - 1) ()
    in
    results, cycles, cycle
  ;;

  let run_first_pass ?(verbose = false) ?(waves = false) (input_coefs : Z.t array array) =
    if verbose
    then print_s [%message (logcores : int) (logblocks : int) (log_passes : int)];
    let sim, waves, inputs, outputs = create_sim waves in
    let input_coefs = Array.map input_coefs ~f:(Array.map ~f:Gf.Z.of_z) in
    let results, cycles, cycle = create_cycle sim outputs in
    start_sim inputs cycle;
    inputs.first_4step_pass := Bits.of_bool true;
    let coefs = convert_to_first_pass_input input_coefs in
    while not (Bits.to_bool !(outputs.data_in_dest.tready)) do
      cycle ()
    done;
    for i = 0 to (Array.length coefs lsr logcores) - 1 do
      while not (Bits.to_bool !(outputs.data_in_dest.tready)) do
        cycle ()
      done;
      inputs.data_in.tvalid := Bits.vdd;
      inputs.data_in.tdata
        := List.init num_cores ~f:(fun j ->
             coefs.((i lsl logcores) + j) |> Gf.Z.to_z |> Gf.Bits.of_z |> Gf.Bits.to_bits)
           |> Bits.concat_lsb;
      cycle ()
    done;
    inputs.data_in.tvalid := Bits.gnd;
    (* A few cycles of flushing after each pass *)
    cycle ~n:4 ();
    (* wait for the core to complete. *)
    while not (Bits.to_bool !(outputs.done_)) do
      cycle ()
    done;
    cycle ~n:4 ();
    print_s [%message (!cycles : int)];
    (try
       let hw_results = get_first_pass_results !results in
       check_first_pass_output ~verbose input_coefs hw_results
     with
     | e -> print_s [%message "RAISED :(" (e : exn)]);
    waves
  ;;

  let run ?verbose ?waves input_coefs = run_first_pass ?verbose ?waves input_coefs
end

let run_test ~waves ~logn ~logcores ~logblocks =
  let module Config = struct
    let logn = logn
    let logcores = logcores
    let logblocks = logblocks
    let support_4step_twiddle = true
    let memory_layout = Zprize_ntt.Memory_layout.Optimised_layout_single_port
  end
  in
  let module Test = Make (Config) in
  Test.run ~waves (Test.random_input_coef_matrix ())
;;

let%expect_test "single core" =
  let waves = run_test ~waves:false ~logn:4 ~logcores:0 ~logblocks:0 in
  Option.iter
    waves
    ~f:
      (Waveform.print
         ~start_cycle:60
         ~display_width:94
         ~display_height:80
         ~wave_width:(-1));
  [%expect
    {|
    (!cycles 1661)
    ("Hardware and software reference results match!" (pass first)) |}]
;;

let%expect_test "8 cores" =
  ignore (run_test ~waves:false ~logn:4 ~logcores:3 ~logblocks:0 : Waveform.t option);
  [%expect
    {|
    (!cycles 247)
    ("Hardware and software reference results match!" (pass first)) |}]
;;

let%expect_test "2 cores, 2 blocks" =
  ignore (run_test ~waves:false ~logn:4 ~logcores:1 ~logblocks:1 : Waveform.t option);
  [%expect
    {|
    (!cycles 481)
    ("Hardware and software reference results match!" (pass first)) |}]
;;

let%expect_test "4 cores, 4 blocks, twiddles 1st and 2nd stages" =
  ignore (run_test ~waves:false ~logn:5 ~logcores:2 ~logblocks:2 : Waveform.t option);
  [%expect
    {|
    (!cycles 617)
    ("Hardware and software reference results match!" (pass first)) |}]
;;

let%expect_test "other configurations with twiddles" =
  ignore (run_test ~waves:false ~logn:4 ~logcores:0 ~logblocks:0 : Waveform.t option);
  ignore (run_test ~waves:false ~logn:5 ~logcores:0 ~logblocks:2 : Waveform.t option);
  ignore (run_test ~waves:false ~logn:5 ~logcores:2 ~logblocks:0 : Waveform.t option);
  ignore (run_test ~waves:false ~logn:5 ~logcores:1 ~logblocks:3 : Waveform.t option);
  ignore (run_test ~waves:false ~logn:5 ~logcores:3 ~logblocks:1 : Waveform.t option);
  [%expect
    {|
    (!cycles 1661)
    ("Hardware and software reference results match!" (pass first))
    (!cycles 1661)
    ("Hardware and software reference results match!" (pass first))
    (!cycles 1469)
    ("Hardware and software reference results match!" (pass first))
    (!cycles 1057)
    ("Hardware and software reference results match!" (pass first))
    (!cycles 489)
    ("Hardware and software reference results match!" (pass first)) |}]
;;
