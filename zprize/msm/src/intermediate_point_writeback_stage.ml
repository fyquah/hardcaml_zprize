open Hardcaml
open Hardcaml_xilinx
open Signal

module Make (Config : Config.S) (Stage_interfaces : Stage_interfaces.M(Config).S) = struct
  open Stage_interfaces.Intermediate_point_writeback_stage

  let latency = 1

  let create
      ~clock
      ~intermediate_points_memory
      ~(point_adding_stage : _ Stage_interfaces.Adding.Stage_output.t)
      ~(point_doubling_stage : _ Stage_interfaces.Doubling.Stage_output.t)
    =
    let use_point_doubling_stage =
      point_doubling_stage.valid &: point_doubling_stage.is_handling_doubling_fault
    in
    let write_port =
      { Memory_builder.Write_port_1d.address =
          mux2
            use_point_doubling_stage
            point_doubling_stage.entry_index
            point_adding_stage.entry_index
      ; enable =
          point_adding_stage.valid
          |: (point_doubling_stage.intermediate_point_write_to_buffer
             &: point_doubling_stage.valid)
      ; data =
          Jacobian_point_with_metadata.Of_signal.mux2
            use_point_doubling_stage
            point_doubling_stage.intermediate_point
            point_adding_stage.intermediate_point
      }
    in
    Memory_builder.set_write_port_1d intermediate_points_memory B write_port;
    { Stage_output.entry_index = write_port.address
    ; intermediate_point = write_port.data
    ; valid =
        point_adding_stage.valid
        |: (point_doubling_stage.valid &: point_doubling_stage.is_handling_doubling_fault)
    }
    |> Stage_output.map ~f:(Signal.pipeline ~n:latency (Reg_spec.create ~clock ()))
  ;;
end
