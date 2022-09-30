open! Core
open Hardcaml
open! Hardcaml_waveterm
module Gf = Hardcaml_ntt.Gf

module Make (Config : Hardcaml_ntt.Core_config.S) = struct
  open Config
  module Kernel = Zprize_ntt.For_vitis.Make (Config)
  module Reference_model = Hardcaml_ntt.Reference_model.Make (Gf.Z)
  module Test_top = Test_top.Make (Config)
  module Sim = Cyclesim.With_interface (Kernel.I) (Kernel.O)
  module VSim = Hardcaml_verilator.With_interface (Kernel.I) (Kernel.O)

  (* Derived parameters *)
  let n = 1 lsl logn
  let num_cores = 1 lsl logcores
  let log_passes = logn - logcores
  let num_passes = 1 lsl log_passes
  let logtotal = logblocks + logcores
  let () = assert (1 lsl (logn + logn) = n * num_cores * num_passes)

  (* Common functions *)
  let random_input_coef_matrix = Test_top.random_input_coef_matrix
  let print_matrix = Test_top.print_matrix
  let copy_matrix = Test_top.copy_matrix
  let grouped n l = List.groupi l ~break:(fun i _ _ -> i % n = 0)
  let random_bool ~p_true = Float.(Random.float 1.0 < p_true)

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

  (* Check the results of the 1st pass against sw.  raise if bad. *)
  let expected_first_pass ~verbose input_coefs hw_results =
    let module Model = Hardcaml_ntt.Reference_model.Make (Gf.Z) in
    let coefs = Model.transpose (copy_matrix input_coefs) in
    Array.iter coefs ~f:Model.inverse_dif;
    Test_top.twiddle coefs;
    let coefs = Model.transpose coefs in
    if verbose
    then (
      printf "\ninter sw\n\n";
      print_matrix coefs;
      printf "\ninter hw\n\n";
      print_matrix hw_results);
    if not ([%equal: Gf.Z.t array array] hw_results coefs)
    then raise_s [%message "intermediate results dont match"]
  ;;

  (* Check the final results using a standard full size ntt. *)
  let expected ~verbose input_coefs hw_results =
    let sw_results = reference_intt input_coefs in
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

  let read_words inputs ~row ~col =
    List.init (1 lsl logcores) ~f:(fun c -> inputs.(row).(col + c))
  ;;

  (* Read a square block of [2^logtotal].  Used to read data in the first pass. *)
  let read_block_transposed_pass ~block_row ~block_col (inputs : 'a array array) =
    Array.init (1 lsl logtotal) ~f:(fun row ->
      Array.init (1 lsl logblocks) ~f:(fun col ->
        read_words
          inputs
          ~row:((block_row lsl logtotal) + row)
          ~col:((block_col lsl logtotal) + (col lsl logcores))))
  ;;

  (* Read a rectangle of [2^locores by 2^logtotal] used in the 2nd pass. *)
  let read_block_linear_pass ~block_row ~block_col (inputs : 'a array array) =
    Array.init (1 lsl logcores) ~f:(fun row ->
      Array.init (1 lsl logblocks) ~f:(fun col ->
        read_words
          inputs
          ~row:((block_row lsl logcores) + row)
          ~col:((block_col lsl logtotal) + (col lsl logcores))))
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
      if !x > 10_000 then raise_s [%message "Running too long"]
    in
    start_sim inputs cycle;
    let run_pass ~which coefs =
      let controller_to_compute =
        match which with
        | `First -> inputs.controller_to_compute_phase_1
        | `Second -> inputs.controller_to_compute_phase_2
      in
      let controller_to_compute_dest =
        match which with
        | `First -> outputs.controller_to_compute_phase_1_dest
        | `Second -> outputs.controller_to_compute_phase_2_dest
      in
      num_results := 0;
      results := [];
      (match which with
       | `First ->
         (* cheat - force the core to [start] *)
         controller_to_compute.tvalid := Bits.vdd;
         cycle ();
         (* wait for tready *)
         assert (Bits.to_bool !(controller_to_compute_dest.tready));
         for block_col = 0 to (n lsr logtotal) - 1 do
           for block_row = 0 to (n lsr logtotal) - 1 do
             let block = read_block_transposed_pass ~block_row ~block_col coefs in
             Array.iter
               block
               ~f:
                 (Array.iter ~f:(fun coefs ->
                    controller_to_compute.tvalid := Bits.gnd;
                    while not (random_bool ~p_true:wiggle_prob) do
                      cycle ()
                    done;
                    controller_to_compute.tvalid := Bits.vdd;
                    controller_to_compute.tdata
                      := coefs
                         |> List.map ~f:(fun z ->
                              Gf.Bits.to_bits (Gf.Bits.of_z (Gf.Z.to_z z)))
                         |> Bits.concat_lsb;
                    while Bits.is_gnd !(controller_to_compute_dest.tready) do
                      cycle ()
                    done;
                    cycle ()))
           done;
           controller_to_compute.tvalid := Bits.gnd
           (* A few cycles of flushing after each pass *)
         done
       | `Second ->
         for block_row = 0 to (n lsr logcores) - 1 do
           for block_col = 0 to (n lsr logtotal) - 1 do
             let block = read_block_linear_pass ~block_row ~block_col coefs in
             Array.iter
               block
               ~f:
                 (Array.iter ~f:(fun coefs ->
                    while not (random_bool ~p_true:wiggle_prob) do
                      controller_to_compute.tvalid := Bits.gnd;
                      cycle ()
                    done;
                    controller_to_compute.tvalid := Bits.vdd;
                    controller_to_compute.tdata
                      := coefs
                         |> List.map ~f:(fun z ->
                              Gf.Bits.to_bits (Gf.Bits.of_z (Gf.Z.to_z z)))
                         |> Bits.concat_lsb;
                    while Bits.is_gnd !(controller_to_compute_dest.tready) do
                      cycle ()
                    done;
                    cycle ()))
           done;
           controller_to_compute.tvalid := Bits.gnd;
           (* A few cycles of flushing after each pass *)
           cycle ~n:4 ()
         done);
      (* wait for the core to return all results. *)
      while !num_results <> n * n / num_cores do
        cycle ()
      done;
      get_results !results
    in
    (try
       let pass1 = run_pass ~which:`First input_coefs in
       expected_first_pass ~verbose input_coefs pass1;
       let pass2 = run_pass ~which:`Second pass1 in
       cycle ~n:4 ();
       expected ~verbose input_coefs pass2
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
  end
  in
  let module Test = Make (Config) in
  let input_coefs = Test.random_input_coef_matrix () in
  ignore
    (Test.run ~verilator:false ~verbose:false ~waves:false input_coefs
      : Waveform.t option);
  [%expect {|
    "Hardware and software reference results match!" |}]
;;

let%expect_test "2 blocks" =
  let module Config = struct
    let logn = 5
    let logcores = 3
    let logblocks = 1
    let support_4step_twiddle = true
  end
  in
  let module Test = Make (Config) in
  let input_coefs = Test.random_input_coef_matrix () in
  ignore
    (Test.run ~verilator:false ~verbose:false ~waves:false input_coefs
      : Waveform.t option);
  [%expect {| "Hardware and software reference results match!" |}]
;;
