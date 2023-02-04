open Core

module Synthesis_strategy = struct
  type t =
    | Flow_AreaOptimized_high
    | Flow_AreaOptimized_medium
    | Flow_AreaMultThresholdDSP
    | Flow_AlternateRoutability
    | Flow_PerfOptimized_high
    | Flow_PerfThresholdCarry
    | Flow_RuntimeOptimized
  [@@deriving sexp_of]

  let to_string t = Sexp.to_string_hum (sexp_of_t t)
end

module Implementation_strategy = struct
  type t =
    | Performance_Explore
    | Performance_ExplorePostRoutePhysOpt
    | Congestion_SSI_SpreadLogic_high
  [@@deriving sexp_of]

  let to_string t = Sexp.to_string_hum (sexp_of_t t)
end

module Opt_design_directive = struct
  type t =
    | Explore
    | ExploreArea
    | AddRemap
    | ExploreSequentialArea
    | RuntimeOptimized
    | NoBramPowerOpt
    | ExploreWithRemap
    | Default
  [@@deriving sexp_of]

  let to_string t = Sexp.to_string_hum (sexp_of_t t)
end

module Place_design_directive = struct
  type t =
    | Explore
    | WLDrivenBlockPlacement
    | EarlyBlockPlacement
    | ExtraNetDelay_high
    | ExtraNetDelay_low
    | SSI_SpreadLogic_high
    | SSI_SpreadLogic_low
    | AltSpreadLogic_high
    | AltSpreadLogic_medium
    | AltSpreadLogic_low
    | ExtraPostPlacementOpt
    | ExtraTimingOpt
    | SSI_SpreadSLLs
    | SSI_BalanceSLLs
    | SSI_BalanceSLRs
    | SSI_HighUtilSLRs
    | RuntimeOptimized
    | Quick
    | Default
  [@@deriving sexp_of]

  let to_string t = Sexp.to_string_hum (sexp_of_t t)
end

module Phys_opt_design_directive = struct
  type t =
    | Explore
    | ExploreWithHoldFix
    | ExploreWithAggressiveHoldFix
    | AggressiveExplore
    | AlternateReplication
    | AggressiveFanoutOpt
    | AddRetime
    | AlternateFlowWithRetiming
    | Default
    | RuntimeOptimized
  [@@deriving sexp_of]

  let to_string t = Sexp.to_string_hum (sexp_of_t t)
end

module Route_design_directive = struct
  type t =
    | Explore
    | AggressiveExplore
    | NoTimingRelaxation
    | MoreGlobalIterations
    | HigherDelayCost
    | RuntimeOptimized
    | AlternateCLBRouting
    | Quick
    | Default
  [@@deriving sexp_of]

  let to_string t = Sexp.to_string_hum (sexp_of_t t)
end

module Linker_config_args = struct
  type t =
    { kernel_frequency : int
    ; synthesis_strategy : Synthesis_strategy.t option
    ; implementation_strategy : Implementation_strategy.t option
    ; opt_design_directive : Opt_design_directive.t option
    ; place_design_directive : Place_design_directive.t option
    ; phys_opt_design_directive : Phys_opt_design_directive.t option
    ; route_design_directive : Route_design_directive.t option
    ; route_design_tns_cleanup : bool
    ; post_route_phys_opt_design_directive : Phys_opt_design_directive.t option
    }
end

let write_linker_config
  { Linker_config_args.synthesis_strategy
  ; implementation_strategy
  ; opt_design_directive
  ; route_design_directive
  ; place_design_directive
  ; phys_opt_design_directive
  ; kernel_frequency
  ; post_route_phys_opt_design_directive
  ; route_design_tns_cleanup
  }
  ~output_string
  =
  output_string
    [%string
      {|kernel_frequency=%{kernel_frequency#Int}
report_dir=./reports
log_dir=./logs

[connectivity]
nk=krnl_mm2s:2
stream_connect=krnl_mm2s_1.out:krnl_msm_pippenger_1.ddr_points_to_fpga
stream_connect=krnl_mm2s_2.out:krnl_msm_pippenger_1.host_scalars_to_fpga
stream_connect=krnl_msm_pippenger_1.fpga_to_host:krnl_s2mm_1.in

[vivado]
# The property below will force xsim to trace all signals in simulation
prop=fileset.sim_1.xsim.elaborate.debug_level=all
prop=run.impl_1.STEPS.PLACE_DESIGN.TCL.PRE=pre_place.tcl
|}];
  Option.iter synthesis_strategy ~f:(fun synthesis_strategy ->
    output_string
      [%string "prop=run.synth_1.STRATEGY=%{synthesis_strategy#Synthesis_strategy}\n"]);
  Option.iter implementation_strategy ~f:(fun implementation_strategy ->
    output_string
      [%string
        "prop=run.impl_1.STRATEGY=%{implementation_strategy#Implementation_strategy}\n"]);
  if route_design_tns_cleanup
  then
    output_string
      [%string "prop=run.impl_1.{STEPS.ROUTE_DESIGN.ARGS.MORE OPTIONS}=-tns_cleanup\n"];
  let write_impl_directive name to_string directive =
    Option.iter directive ~f:(fun x ->
      output_string
        [%string "prop=run.impl_1.STEPS.%{name}.ARGS.DIRECTIVE=%{to_string x}\n"])
  in
  write_impl_directive "OPT_DESIGN" Opt_design_directive.to_string opt_design_directive;
  write_impl_directive
    "PLACE_DESIGN"
    Place_design_directive.to_string
    place_design_directive;
  write_impl_directive
    "PHYS_OPT_DESIGN"
    Phys_opt_design_directive.to_string
    phys_opt_design_directive;
  write_impl_directive
    "ROUTE_DESIGN"
    Route_design_directive.to_string
    route_design_directive;
  write_impl_directive
    "POST_ROUTE_PHYS_OPT_DESIGN"
    Phys_opt_design_directive.to_string
    post_route_phys_opt_design_directive
;;

let%expect_test "Demo linker config output" =
  write_linker_config
    { synthesis_strategy = Some Flow_AlternateRoutability
    ; implementation_strategy = Some Congestion_SSI_SpreadLogic_high
    ; opt_design_directive = Some Explore
    ; route_design_directive = Some AlternateCLBRouting
    ; place_design_directive = Some Explore
    ; phys_opt_design_directive = Some AggressiveExplore
    ; kernel_frequency = 420
    ; post_route_phys_opt_design_directive = Some AggressiveExplore
    ; route_design_tns_cleanup = true
    }
    ~output_string:Stdio.print_string;
  [%expect
    {|
    kernel_frequency=420
    report_dir=./reports
    log_dir=./logs

    [connectivity]
    nk=krnl_mm2s:2
    stream_connect=krnl_mm2s_1.out:krnl_msm_pippenger_1.ddr_points_to_fpga
    stream_connect=krnl_mm2s_2.out:krnl_msm_pippenger_1.host_scalars_to_fpga
    stream_connect=krnl_msm_pippenger_1.fpga_to_host:krnl_s2mm_1.in

    [vivado]
    # The property below will force xsim to trace all signals in simulation
    prop=fileset.sim_1.xsim.elaborate.debug_level=all
    prop=run.impl_1.STEPS.PLACE_DESIGN.TCL.PRE=pre_place.tcl
    prop=run.synth_1.STRATEGY=Flow_AlternateRoutability
    prop=run.impl_1.STRATEGY=Congestion_SSI_SpreadLogic_high
    prop=run.impl_1.{STEPS.ROUTE_DESIGN.ARGS.MORE OPTIONS}=-tns_cleanup
    prop=run.impl_1.STEPS.OPT_DESIGN.ARGS.DIRECTIVE=Explore
    prop=run.impl_1.STEPS.PLACE_DESIGN.ARGS.DIRECTIVE=Explore
    prop=run.impl_1.STEPS.PHYS_OPT_DESIGN.ARGS.DIRECTIVE=AggressiveExplore
    prop=run.impl_1.STEPS.ROUTE_DESIGN.ARGS.DIRECTIVE=AlternateCLBRouting
    prop=run.impl_1.STEPS.POST_ROUTE_PHYS_OPT_DESIGN.ARGS.DIRECTIVE=AggressiveExplore |}]
;;

let timing_summary_pattern =
  lazy
    (Re.Perl.compile
       (Re.Perl.re
          "Timing Summary \\| WNS=(-?\\d+(?:\\.\\d+)) \\| TNS=(-?\\d+(?:\\.\\d+)) \\| \
           WHS=(-?\\d+(?:\\.\\d+)) \\| THS=(-?\\d+(?:\\.\\d+)) |\\s*$"))
;;

module Timing_summary = struct
  type t =
    { wns : Bignum.t
    ; tns : Bignum.t
    ; whs : Bignum.t
    ; ths : Bignum.t
    }
  [@@deriving sexp_of]

  let achieved_frequency ~compile_frequency_in_mhz t =
    if Bignum.(t.wns >= zero)
    then compile_frequency_in_mhz
    else (
      let compile_period_in_nanos =
        Bignum.(inverse compile_frequency_in_mhz * of_int 1_000)
      in
      let period_in_nanos = Bignum.(compile_period_in_nanos + neg t.wns) in
      Bignum.(inverse (period_in_nanos / of_int 1_000)))
  ;;
end

let parse_vivado_logs_for_timing_summary ~vivado_log_lines =
  List.find_map (List.rev vivado_log_lines) ~f:(fun line ->
    let matches = Re.exec (Lazy.force timing_summary_pattern) (String.strip line) in
    let matches = Re.Group.all matches in
    if String.is_empty matches.(0)
    then None
    else (
      let wns = Bignum.of_string matches.(1) in
      let tns = Bignum.of_string matches.(2) in
      let whs = Bignum.of_string matches.(3) in
      let ths = Bignum.of_string matches.(4) in
      Some { Timing_summary.wns; tns; whs; ths }))
;;

let%expect_test "Parse some timing summaries" =
  Stdio.print_s
    ([%sexp_of: Timing_summary.t option]
       (parse_vivado_logs_for_timing_summary
          ~vivado_log_lines:
            [ "INFO: [Physopt 32-668] Current Timing Summary | WNS=-0.025 | TNS=-1.456 | \
               WHS=0.004 | THS=0.000 |"
            ; "INFO: [Physopt 32-669] Post Physical Optimization Timing Summary | \
               WNS=-0.025 | TNS=-1.456 | WHS=0.004 | THS=0.000 |"
            ; "INFO: [Route 35-253] TNS is the sum of the worst slack violation on every \
               endpoint in the design. Review the paths with the biggest WNS violations \
               in the timing reports and modify your constraints or your design to \
               improve both WNS and TNS."
            ; "| Pass |  WNS   |  TNS   |  WHS   |  THS   | Status | Elapsed Time | \
               Solution Selected"
            ]));
  [%expect {| (((wns -0.025) (tns -1.456) (whs 0.004) (ths 0))) |}]
;;

let%expect_test "Achieved frequency when failing timing" =
  Stdio.print_string
    (Bignum.to_string_hum
       (Timing_summary.achieved_frequency
          ~compile_frequency_in_mhz:(Bignum.of_int 280)
          { wns = Bignum.of_string "-0.025"
          ; tns = Bignum.of_string "-1.456"
          ; whs = Bignum.of_string "0.004"
          ; ths = Bignum.of_string "0"
          }));
  [%expect {| 278.053624628 |}]
;;

let%expect_test "Achieved frequency when passing timing" =
  Stdio.print_string
    (Bignum.to_string_hum
       (Timing_summary.achieved_frequency
          ~compile_frequency_in_mhz:(Bignum.of_int 280)
          { wns = Bignum.of_string "0.2"
          ; tns = Bignum.of_string "0"
          ; whs = Bignum.of_string "0.004"
          ; ths = Bignum.of_string "0"
          }));
  [%expect {| 280 |}]
;;
