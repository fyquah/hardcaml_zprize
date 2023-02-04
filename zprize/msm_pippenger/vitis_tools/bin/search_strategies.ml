open Core
open Async

module Build_result = struct
  type t =
    { wns : int
    ; whs : int
    }
end

let experiments =
  let open Msm_pippenger.Vitis_util.Linker_config_args in
  [ { synthesis_strategy = Some Flow_AlternateRoutability
    ; implementation_strategy = Some Congestion_SSI_SpreadLogic_high
    ; opt_design_directive = Some Explore
    ; route_design_directive = Some AlternateCLBRouting
    ; place_design_directive = Some Explore
    ; phys_opt_design_directive = Some AggressiveExplore
    ; kernel_frequency = 310
    ; post_route_phys_opt_design_directive = Some AggressiveExplore
    ; route_design_tns_cleanup = true
    }
  ]
;;

let run_build ~template_dir ~build_dir ~build_id ~linker_config =
  let build_dir = build_dir ^/ build_id in
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
    let%bind wrt = Writer.open_file (build_dir ^/ "msm_pippenger.cfg") in
    Msm_pippenger.Vitis_util.write_linker_config
      linker_config
      ~output_string:(Writer.write wrt);
    Writer.close wrt
  in
  (* Run ./compile_hw.sh from build_dir/build_id *)
  let%bind.Deferred.Or_error (_ : string) =
    Process.run ~working_dir:build_dir ~prog:"./compile_hw.sh" ~args:[] ()
  in
  (* Parse vivado.log to load the WNS or something? *)
  return (Ok ())
;;

let command_build =
  Command.async
    ~summary:
      "Spawns various builds of the msm with various strategies and report the \
       implementation results in the end."
    (let%map_open.Command build_dir =
       flag "build-dir" (required string) ~doc:" Directory to perform builds in"
     and template_dir = flag "template-dir" (required string) ~doc:" Template directory"
     and max_jobs =
       flag
         "max-jobs"
         (required int)
         ~doc:" Max number of parallel build jobs to run concurrently"
     in
     fun () ->
       if not (Sys_unix.file_exists_exn template_dir)
       then raise_s [%message "Template does not exist!" (template_dir : string)];
       let%bind results =
         Deferred.List.mapi
           experiments
           ~how:(`Max_concurrent_jobs max_jobs)
           ~f:(fun i linker_config ->
           run_build
             ~template_dir
             ~build_dir
             ~build_id:("build-" ^ Int.to_string i)
             ~linker_config)
       in
       return ())
;;

let () = Command.group ~summary:"" [ "build", command_build ] |> Command_unix.run
