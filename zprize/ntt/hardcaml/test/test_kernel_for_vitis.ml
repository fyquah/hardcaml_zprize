open! Core
open Hardcaml
open! Hardcaml_waveterm
module N4 = Ntts_r_fun.Ntt_4step
module Gf_z = Ntts_r_fun.Gf_z
module Gf_bits = Ntts_r_fun.Gf_bits.Make (Bits)

module Make (Config : Ntts_r_fun.Ntt.Config) = struct
  module Ntt_4step = Ntts_r_fun.Ntt_4step.Make (Config)
  module Ntt_sw = Ntts_r_fun.Ntt_sw.Make (Gf_z)
  module Kernel = Ntt_4step.Kernel_for_vitis
  module Test_kernel = Test_kernel.Make (Config)
  module Sim = Cyclesim.With_interface (Kernel.I) (Kernel.O)

  (* Derived parameters *)
  let logn = Config.logn
  let n = 1 lsl logn
  let logcores = Ntt_4step.logcores
  let num_cores = 1 lsl logcores
  let log_passes = logn - logcores
  let num_passes = 1 lsl log_passes
  let () = assert (1 lsl (logn + logn) = n * num_cores * num_passes)

  (* Common functions *)
  let random_input_coef_matrix = Test_kernel.random_input_coef_matrix
  let print_matrix = Test_kernel.print_matrix
  let copy_matrix = Test_kernel.copy_matrix
  let get_results = Test_kernel.get_results
  let transpose = Ntt_sw.transpose

  let create_sim waves =
    let sim =
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
    if [%equal: Gf_z.t array array] hw_results sw_results
    then print_s [%message "Hardware and software reference results match!"]
    else raise_s [%message "ERROR: Hardware and software results do not match :("]
  ;;

  let run ?(verbose = false) ?(waves = false) (input_coefs : Z.t array array) =
    let sim, waves, inputs, outputs = create_sim waves in
    let input_coefs = Array.map input_coefs ~f:(Array.map ~f:Gf_z.of_z) in
    let results = ref [] in
    let num_results = ref 0 in
    let cycle ?(n = 1) () =
      assert (n > 0);
      if Bits.to_bool !(outputs.compute_to_controller.tvalid)
      then (
        results := !(outputs.compute_to_controller.tdata) :: !results;
        Int.incr num_results);
      for _ = 1 to n do
        Cyclesim.cycle sim
      done
    in
    start_sim inputs cycle;
    let run_pass coefs =
      (* cheat - force the core to [start] *)
      inputs.controller_to_compute.tvalid := Bits.vdd;
      cycle ();
      num_results := 0;
      results := [];
      for pass = 0 to num_passes - 1 do
        (* wait for tready *)
        while not (Bits.to_bool !(outputs.controller_to_compute_dest.tready)) do
          cycle ()
        done;
        for i = 0 to n - 1 do
          inputs.controller_to_compute.tvalid := Bits.vdd;
          inputs.controller_to_compute.tdata
            := List.init num_cores ~f:(fun core -> coefs.((pass * num_cores) + core).(i))
               |> List.map ~f:(fun z -> Gf_bits.to_bits (Gf_bits.of_z (Gf_z.to_z z)))
               |> Bits.concat_lsb;
          cycle ()
        done;
        inputs.controller_to_compute.tvalid := Bits.gnd;
        (* tready should be low. *)
        assert (not (Bits.to_bool !(outputs.controller_to_compute_dest.tready)));
        (* A few cycles of flushing after each pass *)
        cycle ~n:4 ()
      done;
      (* wait for the core to return all results. *)
      while !num_results <> n * n / num_cores do
        cycle ()
      done;
      get_results !results
    in
    let pass1 = run_pass (transpose input_coefs) in
    let pass2 = transpose (run_pass (transpose pass1)) in
    cycle ~n:4 ();
    (try expected ~verbose input_coefs pass2 with
    | e -> print_s [%message "RAISED :(" (e : exn)]);
    waves
  ;;
end
