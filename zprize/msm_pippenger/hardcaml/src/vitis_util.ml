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
  oc
  =
  Stdio.Out_channel.output_string
    oc
    [%string
      {|
kernel_frequency=%{kernel_frequency#Int}
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
    Stdio.Out_channel.output_lines
      oc
      [ [%string "prop=run.synth_1.STRATEGY=%{synthesis_strategy#Synthesis_strategy}"] ]);
  Option.iter implementation_strategy ~f:(fun implementation_strategy ->
    Stdio.Out_channel.output_lines
      oc
      [ [%string
        "prop=run.impl_1.STRATEGY=%{implementation_strategy#Implementation_strategy}"] ]);
  if route_design_tns_cleanup
  then
    Stdio.Out_channel.output_lines
      oc
      [ [%string "prop=run.impl_1.{STEPS.ROUTE_DESIGN.ARGS.MORE OPTIONS}=-tns_cleanup"] ];
  let write_impl_directive name to_string directive =
    Option.iter directive ~f:(fun x ->
      Stdio.Out_channel.output_lines
        oc
        [ [%string "prop=run.impl_1.STEPS.%{name}.ARGS.DIRECTIVE=%{to_string x}"] ])
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
  Stdio.Out_channel.stdout;
  [%expect {|
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
