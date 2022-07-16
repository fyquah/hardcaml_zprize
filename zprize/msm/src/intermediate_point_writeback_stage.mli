open Hardcaml
open Hardcaml_xilinx

module Make (Config : Config.S) (Stage_interfaces : Stage_interfaces.M(Config).S) : sig
  open Stage_interfaces.Intermediate_point_writeback_stage

  val latency : int

  val create
    :  clock:Signal.t
    -> intermediate_points_memory:Signal.t Jacobian_point_with_metadata.t Memory_builder.t
    -> point_adding_stage:Signal.t Stage_interfaces.Adding.Stage_output.t
    -> point_doubling_stage:Signal.t Stage_interfaces.Doubling.Stage_output.t
    -> Signal.t Stage_output.t
end
