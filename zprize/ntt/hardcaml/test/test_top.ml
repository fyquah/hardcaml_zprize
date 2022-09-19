open Core
open Hardcaml
open Hardcaml_waveterm
module Gf = Hardcaml_ntt.Gf

module Make (Config : Hardcaml_ntt.Four_step_config.S) = struct
  module Reference_model = Hardcaml_ntt.Reference_model.Make (Gf.Z)
  module Top = Zprize_ntt.Top.Make (Config)
  module Sim = Cyclesim.With_interface (Top.I) (Top.O)

  let logn = Config.logn
  let n = 1 lsl logn
  let logcores = Config.logcores
  let num_cores = 1 lsl logcores
  let log_passes = logn - logcores
  let num_passes = 1 lsl log_passes
  let () = assert (1 lsl (logn + logn) = n * num_cores * num_passes)

  let random_input_coef_matrix () =
    Array.init (1 lsl logn) ~f:(fun _ ->
      Array.init (1 lsl logn) ~f:(fun _ -> Gf.Z.random () |> Gf.Z.to_z))
  ;;

  let twiddle m =
    Reference_model.apply_twiddles Reference_model.inverse_roots.(logn + logn) m
  ;;

  let print_matrix c =
    Array.iteri c ~f:(fun row c ->
      printf "%.3i| " row;
      Array.iteri c ~f:(fun col c ->
        if col <> 0 && col % 8 = 0 then printf "\n   | ";
        printf "%20s " (Z.to_string (Gf.Z.to_z c)));
      printf "\n")
  ;;

  let copy_matrix c = Array.map c ~f:Array.copy

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

  let get_results results =
    let results =
      List.rev results
      |> List.map ~f:(fun b -> Bits.split_lsb ~part_width:64 b |> Array.of_list)
      |> Array.of_list
    in
    let n = 1 lsl logn in
    if Array.length results <> n * n / num_cores
    then (
      let got = Array.length results in
      raise_s [%message "results length is incorrect" (got : int)]);
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
    let sim, waves, inputs, outputs = create_sim waves in
    let input_coefs = Array.map input_coefs ~f:(Array.map ~f:Gf.Z.of_z) in
    let results = ref [] in
    let cycle ?(n = 1) () =
      assert (n > 0);
      if Bits.to_bool !(outputs.data_out.tvalid)
      then results := !(outputs.data_out.tdata) :: !results;
      for _ = 1 to n do
        Cyclesim.cycle sim
      done
    in
    start_sim inputs cycle;
    inputs.first_4step_pass := Bits.of_bool first_4step_pass;
    for pass = 0 to num_passes - 1 do
      (* wait for tready *)
      while not (Bits.to_bool !(outputs.data_in_dest.tready)) do
        cycle ()
      done;
      for i = 0 to n - 1 do
        inputs.data_in.tvalid := Bits.vdd;
        inputs.data_in.tdata
          := List.init num_cores ~f:(fun core ->
               input_coefs.((pass * num_cores) + core).(i))
             |> List.map ~f:(fun z -> Gf.Bits.to_bits (Gf.Bits.of_z (Gf.Z.to_z z)))
             |> Bits.concat_lsb;
        cycle ()
      done;
      inputs.data_in.tvalid := Bits.gnd;
      (* wait for tready to go low. *)
      while Bits.to_bool !(outputs.data_in_dest.tready) do
        cycle ()
      done;
      (* A few cycles of flushing after each pass *)
      cycle ~n:4 ()
    done;
    (* wait for the core to complete. *)
    while not (Bits.to_bool !(outputs.done_)) do
      cycle ()
    done;
    cycle ~n:4 ();
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
  let log_rows_per_iteration = 3
  let logcores = 3
  let logblocks = 0

  let twiddle_4step_config : Hardcaml_ntt.Core_config.twiddle_4step_config option =
    Some
      { rows_per_iteration = 1 lsl log_rows_per_iteration
      ; log_num_iterations = (logn * 2) - log_rows_per_iteration
      }
  ;;
end

module Test = Make (Config)

let%expect_test "" =
  let waves = Test.run ~first_4step_pass:false (Test.random_input_coef_matrix ()) in
  Option.iter
    waves
    ~f:
      (Waveform.print
         ~start_cycle:60
         ~display_width:94
         ~display_height:80
         ~wave_width:(-1));
  [%expect {|
    "Hardware and software reference results match!" |}]
;;
