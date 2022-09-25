open Core
open Hardcaml
open Hardcaml_waveterm
module Gf = Hardcaml_ntt.Gf

module Make (Config : Hardcaml_ntt.Core_config.S) = struct
  module Reference_model = Hardcaml_ntt.Reference_model.Make (Gf.Z)
  module Top = Zprize_ntt.Top.Make (Config)
  module Sim = Cyclesim.With_interface (Top.I) (Top.O)

  let logn = Config.logn
  let n = 1 lsl logn
  let logcores = Config.logcores
  let logblocks = Config.logblocks
  let logtotal = logblocks + logcores
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

  let grouped n l = List.groupi l ~break:(fun i _ _ -> i % n = 0)

  let get_result_blocks (results : Bits.t list) =
    let results =
      List.rev results
      |> List.map ~f:(fun b -> Bits.split_lsb ~part_width:64 b)
      |> List.concat
      |> List.map ~f:(fun b -> Gf.Bits.of_bits b |> Gf.Bits.to_z |> Gf.Z.of_z)
    in
    results
    |> grouped (1 lsl (logtotal + logtotal))
    |> List.map ~f:(fun x ->
         grouped (1 lsl logtotal) x |> List.map ~f:Array.of_list |> Array.of_list)
  ;;

  let get_results (results : Bits.t list) =
    let blocks = ref (get_result_blocks results) in
    let out = Array.init n ~f:(fun _ -> Array.init n ~f:(Fn.const Gf.Z.zero)) in
    for block_col = 0 to (n lsr logtotal) - 1 do
      for block_row = 0 to (n lsr logtotal) - 1 do
        match !blocks with
        | [] -> raise_s [%message "not enough result blocks"]
        | block :: tl ->
          for row = 0 to (1 lsl logtotal) - 1 do
            for col = 0 to (1 lsl logtotal) - 1 do
              out.((block_row lsl logtotal) + row).((block_col lsl logtotal) + col)
                <- block.(row).(col)
            done
          done;
          blocks := tl
      done
    done;
    if not (List.is_empty !blocks) then raise_s [%message "too many blocks"];
    out
  ;;

  let _get_results results =
    let results =
      List.rev results
      |> List.map ~f:(fun b -> Bits.split_lsb ~part_width:64 b |> Array.of_list)
      |> Array.of_list
    in
    let n = 1 lsl logn in
    let got_length =
      Array.fold results ~init:0 ~f:(fun acc results -> acc + Array.length results)
    in
    let expected_length = n * n in
    if got_length <> expected_length
    then
      raise_s
        [%message
          "results length is incorrect" (got_length : int) (expected_length : int)];
    Array.init n ~f:(fun row ->
      let pass = row / num_cores in
      let core = row % num_cores in
      Array.init n ~f:(fun col ->
        let idx = (pass * num_cores * (n / num_cores)) + col in
        Gf.Bits.of_bits results.(idx).(core) |> Gf.Bits.to_z |> Gf.Z.of_z))
  ;;

  let expected ~verbose ~first_4step_pass input_coefs hw_results =
    let sw_results = copy_matrix input_coefs in
    Array.iter sw_results ~f:Reference_model.inverse_dit;
    if first_4step_pass then twiddle sw_results;
    let sw_results = transpose sw_results in
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

  let run
    ?(verbose = false)
    ?(waves = false)
    ~first_4step_pass
    (input_coefs : Z.t array array)
    =
    if verbose
    then print_s [%message (logcores : int) (logblocks : int) (log_passes : int)];
    let sim, waves, inputs, outputs = create_sim waves in
    let input_coefs = Array.map input_coefs ~f:(Array.map ~f:Gf.Z.of_z) in
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
    start_sim inputs cycle;
    inputs.first_4step_pass := Bits.of_bool first_4step_pass;
    if first_4step_pass
    then
      for pass = 0 to (num_passes / num_blocks) - 1 do
        (* wait for tready *)
        while not (Bits.to_bool !(outputs.data_in_dest.tready)) do
          cycle ()
        done;
        for i = 0 to n - 1 do
          for block = 0 to num_blocks - 1 do
            let row_base_index = ((pass * num_blocks) + block) * num_cores in
            inputs.data_in.tvalid := Bits.vdd;
            inputs.data_in.tdata
              := List.init num_cores ~f:(fun core ->
                   input_coefs.(row_base_index + core).(i))
                 |> List.map ~f:(fun z -> Gf.Bits.to_bits (Gf.Bits.of_z (Gf.Z.to_z z)))
                 |> Bits.concat_lsb;
            cycle ()
          done
        done;
        inputs.data_in.tvalid := Bits.gnd;
        (* A few cycles of flushing after each pass *)
        cycle ~n:4 ()
      done
    else
      for pass = 0 to num_passes - 1 do
        (* wait for tready *)
        while not (Bits.to_bool !(outputs.data_in_dest.tready)) do
          cycle ()
        done;
        for i = 0 to n - 1 do
          let row_base_index = pass * num_cores in
          inputs.data_in.tvalid := Bits.vdd;
          inputs.data_in.tdata
            := List.init num_cores ~f:(fun core ->
                 input_coefs.(row_base_index + core).(i))
               |> List.map ~f:(fun z -> Gf.Bits.to_bits (Gf.Bits.of_z (Gf.Z.to_z z)))
               |> Bits.concat_lsb;
          cycle ()
        done;
        inputs.data_in.tvalid := Bits.gnd;
        (* A few cycles of flushing after each pass *)
        cycle ~n:4 ()
      done;
    (* wait for the core to complete. *)
    while not (Bits.to_bool !(outputs.done_)) do
      cycle ()
    done;
    cycle ~n:4 ();
    print_s [%message (!cycles : int)];
    (try
       let hw_results = get_results !results in
       expected ~verbose ~first_4step_pass input_coefs hw_results
     with
     | e -> print_s [%message "RAISED :(" (e : exn)]);
    waves
  ;;
end

module Config = struct
  let logn = 5
  let logcores = 3
  let logblocks = 0
  let support_4step_twiddle = true
end

module Test = Make (Config)

let run_test ~waves ~logn ~logcores ~logblocks ~support_4step_twiddle ~first_4step_pass =
  let module Config = struct
    let logn = logn
    let logcores = logcores
    let logblocks = logblocks
    let support_4step_twiddle = support_4step_twiddle || first_4step_pass
  end
  in
  let module Test = Make (Config) in
  Test.run ~waves ~first_4step_pass (Test.random_input_coef_matrix ())
;;

let%expect_test "single core, no twiddles" =
  let waves =
    run_test
      ~waves:false
      ~logn:4
      ~logcores:0
      ~logblocks:0
      ~support_4step_twiddle:false
      ~first_4step_pass:false
  in
  Option.iter
    waves
    ~f:
      (Waveform.print
         ~start_cycle:60
         ~display_width:94
         ~display_height:80
         ~wave_width:(-1));
  [%expect {|
    (!cycles 1215)
    "Hardware and software reference results match!" |}]
;;

let%expect_test "8 cores, no twiddles" =
  ignore
    (run_test
       ~waves:false
       ~logn:4
       ~logcores:3
       ~logblocks:0
       ~support_4step_twiddle:false
       ~first_4step_pass:false
      : Waveform.t option);
  [%expect {|
    (!cycles 193)
    "Hardware and software reference results match!" |}]
;;

let%expect_test "2 cores, 2 blocks, no twiddles" =
  ignore
    (run_test
       ~waves:false
       ~logn:4
       ~logcores:1
       ~logblocks:1
       ~support_4step_twiddle:false
       ~first_4step_pass:false
      : Waveform.t option);
  [%expect {|
    (!cycles 375)
    "Hardware and software reference results match!" |}]
;;

let%expect_test "4 cores, 4 blocks, twiddles 1st and 2nd stages" =
  ignore
    (run_test
       ~waves:false
       ~logn:5
       ~logcores:2
       ~logblocks:2
       ~support_4step_twiddle:true
       ~first_4step_pass:true
      : Waveform.t option);
  ignore
    (run_test
       ~waves:false
       ~logn:5
       ~logcores:2
       ~logblocks:2
       ~support_4step_twiddle:true
       ~first_4step_pass:false
      : Waveform.t option);
  [%expect
    {|
    (!cycles 623)
    "Hardware and software reference results match!"
    (!cycles 560)
    "Hardware and software reference results match!" |}]
;;

let%expect_test "other configurations with twiddles" =
  ignore
    (run_test
       ~waves:false
       ~logn:5
       ~logcores:0
       ~logblocks:2
       ~support_4step_twiddle:true
       ~first_4step_pass:true
      : Waveform.t option);
  ignore
    (run_test
       ~waves:false
       ~logn:5
       ~logcores:2
       ~logblocks:0
       ~support_4step_twiddle:true
       ~first_4step_pass:true
      : Waveform.t option);
  ignore
    (run_test
       ~waves:false
       ~logn:5
       ~logcores:1
       ~logblocks:3
       ~support_4step_twiddle:true
       ~first_4step_pass:true
      : Waveform.t option);
  ignore
    (run_test
       ~waves:false
       ~logn:5
       ~logcores:3
       ~logblocks:1
       ~support_4step_twiddle:true
       ~first_4step_pass:true
      : Waveform.t option);
  [%expect
    {|
    (!cycles 1697)
    "Hardware and software reference results match!"
    (!cycles 1505)
    "Hardware and software reference results match!"
    (!cycles 1048)
    "Hardware and software reference results match!"
    (!cycles 495)
    "Hardware and software reference results match!" |}]
;;
