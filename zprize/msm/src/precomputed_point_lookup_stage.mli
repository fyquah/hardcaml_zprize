open Hardcaml
open Hardcaml_xilinx

module Make (Config : Config.S) (Stage_interfaces : Stage_interfaces.M(Config).S) : sig
  module Stage_input := Stage_interfaces.Precomputed_point_lookup.Stage_input
  module Stage_output := Stage_interfaces.Precomputed_point_lookup.Stage_output

  val latency : int

  val create
    :  scope:Scope.t
    -> clock:Signal.t
    -> precomputed_points_memory:Signal.t Affine_point_or_infinity.t Memory_builder.t
    -> Signal.t Stage_input.t
    -> Signal.t Stage_output.t
end
