open Core
open Hardcaml
open Hardcaml_waveterm
module N4 = Ntts_r_fun.Ntt_4step
module Gf_z = Ntts_r_fun.Gf_z
module Gf_bits = Ntts_r_fun.Gf_bits.Make (Bits)

module Make (Config : Ntts_r_fun.Ntt.Config) = struct
  module Ntt_4step = Ntts_r_fun.Ntt_4step.Make (Config)
  module Kernel = Ntt_4step.Kernel
  module Sim = Cyclesim.With_interface (Kernel.I) (Kernel.O)

  let logn = Config.logn
  let n = 1 lsl logn
  let logcores = Ntt_4step.logcores
  let num_cores = 1 lsl logcores
  let log_passes = logn - logcores
  let num_passes = 1 lsl log_passes
  let () = assert (1 lsl (logn + logn) = n * num_cores * num_passes)

  let random_input_coefs () =
    List.init (1 lsl (logn + logn)) ~f:(fun _ -> Gf_z.random () |> Gf_z.to_z)
  ;;

  let form_input_coefs coefs =
    let coefs =
      List.groupi coefs ~break:(fun i _ _ -> i mod n = 0) |> List.map ~f:Array.of_list
    in
    let coefs =
      List.groupi coefs ~break:(fun i _ _ -> i mod num_cores = 0)
      |> List.map ~f:Array.of_list
    in
    Array.of_list coefs
  ;;

  let flatten_coefs coefs =
    let a = Array.map coefs ~f:(fun a -> Array.concat (Array.to_list a)) in
    a |> Array.to_list |> Array.concat
  ;;

  let print_matrices c =
    for pass = 0 to num_passes - 1 do
      for i = 0 to n - 1 do
        printf "%.2i:" i;
        for core = 0 to num_cores - 1 do
          let c =
            c.(pass).(core).(i)
            |> Gf_z.to_z
            |> Gf_bits.of_z
            |> Gf_bits.to_bits
            |> Bits.to_constant
            |> Constant.to_hex_string ~signedness:Unsigned
          in
          printf "%s " c
        done;
        printf "\n"
      done;
      printf "\n"
    done
  ;;

  let reference ~verbose input_coefs =
    let module Ntt = Ntts_r_fun.Ntt_sw.Make (Gf_z) in
    let c = Array.map input_coefs ~f:(Array.map ~f:(Array.map ~f:Gf_z.of_z)) in
    if verbose
    then (
      printf "\n\nINPUTS\n\n";
      print_matrices c);
    Array.iter c ~f:(Array.iter ~f:(fun c -> Ntt.inverse_dit c));
    if verbose
    then (
      printf "\n\nREFERENCE\n\n";
      print_matrices c;
      let n =
        Array.fold c ~init:0 ~f:(fun init c ->
          Array.fold ~init c ~f:(fun acc c -> acc + Array.length c))
      in
      let logn = Int.ceil_log2 n in
      let d = flatten_coefs c in
      let m = Ntt.matrix d (logn / 2) (logn / 2) in
      Ntt.apply_twiddles Ntt.inverse_roots.(logn) m;
      printf "\n\nTWIDDLED %i %i\n\n" n logn;
      print_matrices (form_input_coefs (Array.to_list (Array.concat (Array.to_list m)))));
    c
  ;;

  let get_results (results : Bits.t list) =
    let a =
      List.map (List.rev results) ~f:(fun b ->
        List.map (Bits.split_lsb ~part_width:64 b) ~f:(fun x ->
          Bits.to_z ~signedness:Unsigned x |> Gf_z.of_z)
        |> Array.of_list)
      |> List.groupi ~break:(fun i _ _ -> i % n = 0)
      |> List.map ~f:Array.of_list
      |> Array.of_list
    in
    if not (Array.length a = num_passes)
    then
      raise_s
        [%message
          "Invalid number of results?"
            (List.length results : int)
            (Array.length a : int)
            (num_passes : int)];
    Array.init num_passes ~f:(fun pass ->
      Array.init num_cores ~f:(fun core -> Array.init n ~f:(fun n -> a.(pass).(n).(core))))
  ;;

  let run ?(verbose = false) input_coefs =
    Random.init 100;
    let sim =
      Sim.create
        ~config:Cyclesim.Config.trace_all
        (Kernel.create
           ~build_mode:Simulation
           (Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ()))
    in
    let inputs = Cyclesim.inputs sim in
    let outputs = Cyclesim.outputs sim in
    let waves, sim = Waveform.create sim in
    let input_coefs = form_input_coefs input_coefs in
    let results = ref [] in
    let cycle () =
      if Bits.to_bool !(outputs.data_out.tvalid)
      then results := !(outputs.data_out.tdata) :: !results;
      Cyclesim.cycle sim
    in
    inputs.clear := Bits.vdd;
    cycle ();
    inputs.clear := Bits.gnd;
    inputs.data_out_dest.tready := Bits.vdd;
    inputs.start := Bits.vdd;
    cycle ();
    inputs.start := Bits.gnd;
    cycle ();
    for pass = 0 to num_passes - 1 do
      (* wait for tready *)
      while not (Bits.to_bool !(outputs.data_in_dest.tready)) do
        cycle ()
      done;
      for i = 0 to n - 1 do
        inputs.data_in.tvalid := Bits.vdd;
        inputs.data_in.tdata
          := List.init num_cores ~f:(fun core -> input_coefs.(pass).(core).(i))
             |> List.map ~f:(fun z -> Gf_bits.of_z z |> Gf_bits.to_bits)
             |> Bits.concat_lsb;
        cycle ()
      done;
      inputs.data_in.tvalid := Bits.gnd;
      (* wait for tready to go low. *)
      while Bits.to_bool !(outputs.data_in_dest.tready) do
        cycle ()
      done;
      for _ = 0 to 3 do
        cycle ()
      done
    done;
    while not (Bits.to_bool !(outputs.done_)) do
      cycle ()
    done;
    for _ = 0 to 3 do
      cycle ()
    done;
    (try
       let results = get_results !results in
       let reference = reference ~verbose input_coefs in
       if verbose
       then (
         printf "\n\nHW RESULT\n\n";
         print_matrices results);
       if [%equal: Gf_z.t array array array] results reference
       then printf "IT WORKED!!!\n"
       else printf "ERROR!!!\n"
     with
     | e -> print_s [%message "RAISED :(" (e : exn)]);
    waves
  ;;
end

module Config = struct
  let logn = 5
  let log_rows_per_iteration = 3

  let twiddle_4step_config : Ntts_r_fun.Ntt.twiddle_4step_config option =
    Some
      { rows_per_iteration = 1 lsl log_rows_per_iteration
      ; log_num_iterations = (logn * 2) - log_rows_per_iteration
      }
  ;;
end

module Test = Make (Config)

let%expect_test "" =
  let waves = Test.run (Test.random_input_coefs ()) in
  Waveform.print
    ~start_cycle:60
    ~display_width:94
    ~display_height:80
    ~wave_width:(-1)
    waves;
  [%expect
    {|
    ERROR!!!
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────────┐
    │clock             ││╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥│
    │                  ││╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨│
    │clear             ││                                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │                  ││─┬┬┬┬┬┬┬┬┬┬─────────────────────────────────────────────────────────────│
    │data_in_tdata     ││ ││││││││││3F4C04BFF33677E27BA55E49FC4D870E298BE7546898180217652D57DF43.│
    │                  ││─┴┴┴┴┴┴┴┴┴┴─────────────────────────────────────────────────────────────│
    │data_in_tvalid    ││───────────┐                                                            │
    │                  ││           └────────────────────────────────────────────────────────────│
    │data_out_dest_trea││────────────────────────────────────────────────────────────────────────│
    │                  ││                                                                        │
    │start             ││                                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │data_in_dest_tread││───────────┐                                                            │
    │                  ││           └────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────│
    │data_out_tdata    ││ 0000000000000000000000000000000000000000000000000000000000000000000000.│
    │                  ││────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────│
    │data_out_tkeep    ││ FFFFFFFFFFFFFFFF                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │data_out_tlast    ││                                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────│
    │data_out_tstrb    ││ FFFFFFFFFFFFFFFF                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │data_out_tvalid   ││                                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │done_             ││                                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │4STEP             ││────────────────────────────────────────────────────────────────────────│
    │                  ││                                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │controller$ITERATI││ 1                                                                      │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │controller$START_C││                                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │controller$START_I││                                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │controller$START_O││                                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────│
    │controller$STATE  ││ 2                                                                      │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │controller$i$clear││                                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │controller$i$clock││                                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │controller$i$cores││                                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │controller$i$input││           ┌────────────────────────────────────────────────────────────│
    │                  ││───────────┘                                                            │
    │controller$i$outpu││────────────────────────────────────────────────────────────────────────│
    │                  ││                                                                        │
    │controller$i$start││                                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │controller$o$done_││                                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │controller$o$flip ││                                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │controller$o$start││                                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │controller$o$start││                                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │controller$o$start││                                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │gnd               ││                                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │parallel_cores$i$c││                                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │parallel_cores$i$c││                                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │parallel_cores$i$f││────────────────────────────────────────────────────────────────────────│
    │                  ││                                                                        │
    │parallel_cores$i$f││                                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────│
    │parallel_cores$i$r││ 00                                                                     │
    └──────────────────┘└────────────────────────────────────────────────────────────────────────┘ |}]
;;
