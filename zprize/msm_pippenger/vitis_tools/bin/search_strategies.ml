open Core
open Async
module Vitis_utils = Msm_pippenger.Vitis_util

module Which_experiment = struct
  type t = A [@@deriving sexp]

  let arg_type = Command.Arg_type.create (fun x -> t_of_sexp (Atom x))

  let linker_config_args = function
    | A ->
      let synthesis_strategies =
        [ Some Vitis_utils.Synthesis_strategy.Flow_AlternateRoutability
        ; Some Flow_PerfOptimized_high
        ; None
        ]
      in
      List.concat_map synthesis_strategies ~f:(fun synthesis_strategy ->
        [ { Vitis_utils.Linker_config_args.synthesis_strategy
          ; kernel_frequency = 310
          ; implementation_strategy = Some Congestion_SSI_SpreadLogic_high
          ; opt_design_is_enabled = Some true
          ; opt_design_directive = Some Explore
          ; place_design_directive = Some ExtraNetDelay_high
          ; phys_opt_design_is_enabled = Some true
          ; phys_opt_design_directive = Some AggressiveExplore
          ; route_design_directive = Some HigherDelayCost
          ; route_design_tns_cleanup = true
          ; post_route_phys_opt_design_directive = Some AggressiveExplore
          ; post_route_phys_opt_design_is_enabled = Some true
          }
        ; { Vitis_utils.Linker_config_args.synthesis_strategy
          ; kernel_frequency = 310
          ; implementation_strategy = Some Congestion_SSI_SpreadLogic_high
          ; opt_design_is_enabled = Some true
          ; opt_design_directive = Some Explore
          ; place_design_directive = Some SSI_SpreadLogic_high
          ; phys_opt_design_is_enabled = Some true
          ; phys_opt_design_directive = Some AggressiveExplore
          ; route_design_directive = Some AlternateCLBRouting
          ; route_design_tns_cleanup = true
          ; post_route_phys_opt_design_directive = Some AggressiveExplore
          ; post_route_phys_opt_design_is_enabled = Some true
          }
        ; { Vitis_utils.Linker_config_args.synthesis_strategy
          ; kernel_frequency = 310
          ; implementation_strategy = Some Performance_ExplorePostRoutePhysOpt
          ; opt_design_is_enabled = Some true
          ; opt_design_directive = Some Explore
          ; route_design_directive = Some AggressiveExplore
          ; place_design_directive = Some Explore
          ; phys_opt_design_is_enabled = Some true
          ; phys_opt_design_directive = Some AggressiveExplore
          ; route_design_tns_cleanup = true
          ; post_route_phys_opt_design_is_enabled = Some true
          ; post_route_phys_opt_design_directive = Some AggressiveExplore
          }
        ])
  ;;
end

module Build_result = struct
  type t =
    { build_id : string
    ; linker_config_args : Vitis_utils.Linker_config_args.t
    ; timing_summary : Vitis_utils.Timing_summary.t
    }

  let achieved_frequency t =
    Vitis_utils.Timing_summary.achieved_frequency
      ~compile_frequency_in_mhz:(Bignum.of_int t.linker_config_args.kernel_frequency)
      t.timing_summary
  ;;
end

let search_for_vivado_log_filename ~build_dir =
  let%bind.Deferred.Or_error lines =
    Process.run_lines ~prog:"find" ~args:[ build_dir; "-name"; "vivado.log" ] ()
  in
  match
    List.find_map lines ~f:(fun line ->
      let line = String.strip line in
      if Core.String.is_substring ~substring:"link/vivado/vpl/vivado.log" line
      then Some line
      else None)
  with
  | None -> return (Or_error.error_s [%message "Cannot find linker vpl vivado.log"])
  | Some x -> return (Ok x)
;;

let run_build ~template_dir ~build_dir ~build_id ~linker_config =
  (* TOOO(fyquah): Reuse dir from last good build rather than the template *)
  printf "Starting build with build id = %s\n" build_id;
  let build_dir = build_dir ^/ build_id in
  print_s
    [%message
      (build_id : string)
        (build_dir : string)
        (linker_config : Vitis_utils.Linker_config_args.t)];
  let%bind.Deferred.Or_error () =
    match%map Async_unix.Sys.file_exists build_dir with
    | `Yes ->
      Or_error.errorf
        "Cannot start build in a directory that already exists! %s"
        build_dir
    | `No -> Ok ()
    | `Unknown ->
      Or_error.errorf
        "Cannot determine if build dir exists (Incorrect permissions?) %s"
        build_dir
  in
  (* Copy template_dir to build_dir/build_id *)
  let%bind.Deferred.Or_error (_ : string) =
    Process.run ~prog:"cp" ~args:[ "-r"; template_dir; build_dir ] ()
  in
  (* Generate the linker config into msm_pippenger.cfg *)
  let%bind () =
    let linker_filename = build_dir ^/ "msm_pippenger.cfg" in
    printf "Overriding config file in %s\n" linker_filename;
    let%bind wrt = Writer.open_file linker_filename in
    Msm_pippenger.Vitis_util.write_linker_config linker_config ~output_string:(fun line ->
      Writer.write wrt line);
    Writer.close wrt
  in
  (* Run ./compile_hw.sh from build_dir/build_id *)
  let%bind.Deferred.Or_error p =
    Process.create ~working_dir:build_dir ~prog:"./compile_hw.sh" ~args:[] ()
  in
  let pid = Process.pid p in
  printf "Build %s running with pid %s\n" build_id (Pid.to_string pid);
  let%bind output = Process.collect_output_and_wait p in
  let%bind.Deferred.Or_error () =
    match output.exit_status with
    | Ok () -> return (Ok ())
    | Error process_error ->
      let stdout = output.stdout in
      let stderr = output.stderr in
      return
        (Or_error.error_s
           [%message
             "Build failed!"
               (process_error : Core_unix.Exit_or_signal.error)
               (build_id : string)
               (stdout : string)
               (stderr : string)])
  in
  (* Guess the vivado.log in vpl/ *)
  let%bind.Deferred.Or_error vivado_log_filename =
    search_for_vivado_log_filename ~build_dir
  in
  (* Parse vivado.log to load the WNS or something? *)
  let%bind.Deferred.Or_error timing_summary =
    let%map vivado_log_lines = Async.Reader.file_lines vivado_log_filename in
    match Vitis_utils.parse_vivado_logs_for_timing_summary ~vivado_log_lines with
    | None ->
      Or_error.errorf
        "Failed to find timing summary in the vivado log file %s"
        vivado_log_filename
    | Some x -> Ok x
  in
  printf
    !"Completed build (build id: %s) - performance: %{Bignum.to_string_hum}MHz\n"
    build_id
    (Vitis_utils.Timing_summary.achieved_frequency
       timing_summary
       ~compile_frequency_in_mhz:(Bignum.of_int linker_config.kernel_frequency));
  return
    (Ok { Build_result.build_id; linker_config_args = linker_config; timing_summary })
;;

let run_all_builds_for_experiment ~max_jobs ~template_dir ~build_dir ~which_experiment =
  Deferred.List.mapi
    (Which_experiment.linker_config_args which_experiment)
    ~how:(`Max_concurrent_jobs max_jobs)
    ~f:(fun i linker_config ->
    let build_id = "build-" ^ Int.to_string i in
    let%map result = run_build ~template_dir ~build_dir ~build_id ~linker_config in
    (match result with
     | Ok _ -> ()
     | Error e -> print_s [%message "Build failed" (build_id : string) (e : Error.t)]);
    result)
;;

let command_build =
  Command.async
    ~summary:
      "Spawns various builds of the msm with various strategies and report the \
       implementation results in the end."
    (let%map_open.Command build_dir =
       flag "build-dir" (required string) ~doc:" Directory to perform builds in"
     and template_dir = flag "template-dir" (required string) ~doc:" Template directory"
     and which_experiment =
       flag
         "which-experiment"
         (required Which_experiment.arg_type)
         ~doc:" Which experiment to run"
     and max_jobs =
       flag
         "max-jobs"
         (required int)
         ~doc:" Max number of parallel build jobs to run concurrently"
     in
     fun () ->
       if not (Sys_unix.file_exists_exn template_dir)
       then raise_s [%message "Template does not exist!" (template_dir : string)];
       let%bind (_ : string) =
         printf "Building @default in the template_dir %s...\n" template_dir;
         Process.run_exn
           ~prog:"dune"
           ~args:[ "build"; "@" ^ template_dir ^/ "default" ]
           ()
       in
       (* TOOO(fyquah): Dump some summary? *)
       let%bind results =
         run_all_builds_for_experiment
           ~template_dir
           ~build_dir
           ~max_jobs
           ~which_experiment
       in
       let best =
         List.filter_map results ~f:(fun res ->
           match res with
           | Ok res -> Some res
           | Error _ -> None)
         |> List.max_elt ~compare:(fun a b ->
              Bignum.compare
                (Build_result.achieved_frequency a)
                (Build_result.achieved_frequency b))
       in
       Option.iter best ~f:(fun build_result ->
         printf
           !"Best result: %{Bignum.to_string_hum}MHz from build %s\n"
           (Build_result.achieved_frequency build_result)
           build_result.build_id);
       return ())
;;

let () = Command.group ~summary:"" [ "build", command_build ] |> Command_unix.run
