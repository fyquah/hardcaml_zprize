open Core
open Hardcaml
open Hardcaml_xilinx
open Signal

module Make (Config : Config.S) (Stage_interfaces : Stage_interfaces.M(Config).S) = struct
  open Config
  include Stage_interfaces.Precomputed_point_lookup

  let latency = precomputed_points_table_read_latency

  let create
      ~scope
      ~clock
      ~precomputed_points_memory
      { Stage_input.precomputed_point_address
      ; entry_index
      ; intermediate_point
      ; intermediate_point_z_squared
      ; intermediate_point_write_to_buffer = _
      ; is_handling_doubling_fault
      ; valid
      }
    =
    let scope = Scope.sub_scope scope "precomputed_point_lookup_stage" in
    assert (
      Memory_builder.read_latency precomputed_points_memory
      = precomputed_points_table_read_latency);
    let spec = Reg_spec.create ~clock () in
    let valid = valid &: ~:is_handling_doubling_fault in
    let precomputed_point =
      Memory_builder.set_read_port_1d
        precomputed_points_memory
        A
        { address = precomputed_point_address; enable = valid }
    in
    let pipe = pipeline ~n:latency spec in
    { Stage_output.precomputed_point
    ; intermediate_point = Jacobian_point_with_metadata.map ~f:pipe intermediate_point
    ; intermediate_point_z_squared = pipe intermediate_point_z_squared
    ; entry_index = pipe entry_index
    ; valid = pipe valid
    }
    |> Stage_output.map2 Stage_output.port_names ~f:(fun n v ->
           Scope.naming scope v ("stage_output$" ^ n))
  ;;
end
