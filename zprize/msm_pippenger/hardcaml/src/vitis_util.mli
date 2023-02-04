(** Utilities for working with Vitis config files for the MSM
    design.
*)

(* Contents of this file is not specific to the MSM design, but
   so far this is the only project that needs this.
*)

module Synthesis_strategy : sig
  type t =
    | Flow_AreaOptimized_high
    | Flow_AreaOptimized_medium
    | Flow_AreaMultThresholdDSP
    | Flow_AlternateRoutability
    | Flow_PerfOptimized_high
    | Flow_PerfThresholdCarry
    | Flow_RuntimeOptimized
end

module Place_design_directive : sig
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
end

module Implementation_strategy : sig
  type t =
    | Performance_Explore
    | Performance_ExplorePostRoutePhysOpt
    | Congestion_SSI_SpreadLogic_high
end

module Opt_design_directive : sig
  type t =
    | Explore
    | ExploreArea
    | AddRemap
    | ExploreSequentialArea
    | RuntimeOptimized
    | NoBramPowerOpt
    | ExploreWithRemap
    | Default
end

module Route_design_directive : sig
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
end

module Phys_opt_design_directive : sig
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
end

module Linker_config_args : sig
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
  [@@deriving sexp_of]
end

val write_linker_config : Linker_config_args.t -> output_string:(string -> unit) -> unit

module Timing_summary : sig
  type t =
    { wns : Bignum.t
    ; tns : Bignum.t
    ; whs : Bignum.t
    ; ths : Bignum.t
    }
  [@@deriving sexp_of]

  val achieved_frequency : compile_frequency_in_mhz:Bignum.t -> t -> Bignum.t
end

val parse_vivado_logs_for_timing_summary
  :  vivado_log_lines:string list
  -> Timing_summary.t option
