open Core
open Hardcaml
open Hardcaml_xilinx
open Signal

module Make (Config : Config.S) (Stage_interfaces : Stage_interfaces.M(Config).S) = struct
  open Config
  include Stage_interfaces.Intermediate_point_lookup

  let latency = intermediate_points_table_read_latency

  let create
      ~(scope : Scope.t)
      ~clock
      ~intermediate_points_memory
      { Stage_input.precomputed_point_address
      ; entry_index
      ; is_last
      ; is_first
      ; is_handling_doubling_fault
      ; is_streaming_result
      ; valid
      }
    =
    let scope = Scope.sub_scope scope "intermerdiate_point_lookup_stage" in
    assert (
      Memory_builder.read_latency intermediate_points_memory
      = intermediate_points_table_read_latency);
    let spec = Reg_spec.create ~clock () in
    let pipe = pipeline ~n:latency spec in
    let intermediate_point =
      let (from_mem : _ Jacobian_point_with_metadata.t) =
        Memory_builder.set_read_port_1d
          intermediate_points_memory
          A
          { address = entry_index; enable = valid }
      in
      let point =
        { from_mem.point with is_infinity = pipe is_first |: from_mem.point.is_infinity }
      in
      { from_mem with point }
    in
    { Stage_output.precomputed_point_address = pipe precomputed_point_address
    ; intermediate_point
    ; entry_index = pipe entry_index
    ; is_handling_doubling_fault = pipe is_handling_doubling_fault
    ; is_streaming_result = pipe is_streaming_result
    ; is_last = pipe is_last
    ; valid = pipe valid
    }
    |> Stage_output.map2 Stage_output.port_names ~f:(fun n v ->
           Scope.naming scope v ("stage_output$" ^ n))
  ;;
end
