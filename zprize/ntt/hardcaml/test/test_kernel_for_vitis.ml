open! Core
open Hardcaml
open! Hardcaml_waveterm
module Gf = Hardcaml_ntt.Gf

module Make (Config : Hardcaml_ntt.Ntt_4step.Config) = struct
  open Config
  module Kernel = Zprize_ntt.For_vitis.Make (Config)
  module Ntt_sw = Hardcaml_ntt.Ntt_sw.Make (Gf.Z)
  module Test_top = Test_top.Make (Config)
  module Sim = Cyclesim.With_interface (Kernel.I) (Kernel.O)
  module VSim = Hardcaml_verilator.With_interface (Kernel.I) (Kernel.O)

  (* Derived parameters *)
  let n = 1 lsl logn
  let num_cores = 1 lsl logcores
  let log_passes = logn - logcores
  let num_passes = 1 lsl log_passes
  let () = assert (1 lsl (logn + logn) = n * num_cores * num_passes)

  (* Common functions *)
  let random_input_coef_matrix = Test_top.random_input_coef_matrix
  let print_matrix = Test_top.print_matrix
  let copy_matrix = Test_top.copy_matrix
  let get_results = Test_top.get_results
  let transpose = Ntt_sw.transpose

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
    Ntt_sw.inverse_dit coefs;
    Array.init n ~f:(fun row -> Array.init n ~f:(fun col -> coefs.((row * n) + col)))
  ;;

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

  let run
    ?(verbose = false)
    ?(waves = false)
    ?(verilator = false)
    (input_coefs : Z.t array array)
    =
    let sim, waves, inputs, outputs = create_sim ~verilator waves in
    let input_coefs = Array.map input_coefs ~f:(Array.map ~f:Gf.Z.of_z) in
    let results = ref [] in
    let num_results = ref 0 in
    let cycle ?(n = 1) () =
      assert (n > 0);
      for _ = 1 to n do
        if Bits.to_bool !(outputs.compute_to_controller.tvalid)
        then (
          results := !(outputs.compute_to_controller.tdata) :: !results;
          Int.incr num_results);
        Cyclesim.cycle sim
      done
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
         for pass = 0 to num_passes - 1 do
           for i = 0 to n - 1 do
             controller_to_compute.tvalid := Bits.vdd;
             controller_to_compute.tdata
               := List.init num_cores ~f:(fun core ->
                    coefs.((pass * num_cores) + core).(i))
                  |> List.map ~f:(fun z -> Gf.Bits.to_bits (Gf.Bits.of_z (Gf.Z.to_z z)))
                  |> Bits.concat_lsb;
             while Bits.is_gnd !(controller_to_compute_dest.tready) do
               cycle ()
             done;
             cycle ()
           done;
           controller_to_compute.tvalid := Bits.gnd;
           (* assert (not (Bits.to_bool !(controller_to_compute_dest.tready))) *)
           (* A few cycles of flushing after each pass *)
           cycle ~n:4 ()
         done
       | `Second ->
         while not (Bits.to_bool !(controller_to_compute_dest.tready)) do
           cycle ()
         done;
         for i = 0 to (n / 8) - 1 do
           for j = 0 to (n / 8) - 1 do
             for k = 0 to 8 - 1 do
               let indices = List.init 8 ~f:(fun l -> (8 * i) + k, (8 * j) + l) in
               controller_to_compute.tvalid := Bits.vdd;
               controller_to_compute.tdata
                 := List.map indices ~f:(fun (r, c) -> coefs.(r).(c))
                    |> List.map ~f:(fun z -> Gf.Bits.to_bits (Gf.Bits.of_z (Gf.Z.to_z z)))
                    |> Bits.concat_lsb;
               while Bits.is_gnd !(controller_to_compute_dest.tready) do
                 cycle ()
               done;
               cycle ()
             done
           done
         done;
         controller_to_compute.tvalid := Bits.gnd;
         (* A few cycles of flushing after each pass *)
         cycle ~n:4 ());
      (* wait for the core to return all results. *)
      while !num_results <> n * n / num_cores do
        cycle ()
      done;
      get_results !results
    in
    (try
       let pass1 = run_pass ~which:`First (transpose input_coefs) in
       let pass2 = transpose (run_pass ~which:`Second (transpose pass1)) in
       cycle ~n:4 ();
       expected ~verbose input_coefs pass2
     with
     | e -> print_s [%message "RAISED :(" (e : exn)]);
    waves
  ;;
end
