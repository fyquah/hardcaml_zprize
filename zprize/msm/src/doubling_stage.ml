open Hardcaml
open Signal
open Snarks_r_fun

include struct
  open Snarks_r_fun
  module Ec_fpn_dbl = Ec_fpn_dbl
end

module Ec_fpn_dbl_377 = Ec_fpn_dbl.With_interface (struct
  let bits = 377
end)

module Component = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; point_in : 'a Jacobian_point.With_valid.t [@rtlmangle true]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { point_out : 'a Jacobian_point.With_valid.t [@rtlmangle true]
      ; point_out_z_squared : 'a [@bits 377]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let config = Config_presets.For_bls12_377.ec_fpn_ops_with_montgomery_reduction
  let latency = Ec_fpn_dbl_377.latency config

  let create scope { I.clock; point_in = { valid; value = data_in } } : _ O.t =
    let { Ec_fpn_dbl_377.O.valid_out
        ; data_out = { point = data_out; z_squared }
        ; ready_in = _
        }
      =
      Ec_fpn_dbl_377.create
        ~config
        scope
        { clock; enable = vdd; valid_in = valid; data_in }
    in
    { O.point_out = { valid = valid_out; value = data_out }
    ; point_out_z_squared = z_squared
    }
  ;;

  let hierarchical scope i =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~name:"doubling_stage_component" ~scope create i
  ;;
end

module Make (Config : Config.S) (Stage_interfaces : Stage_interfaces.M(Config).S) = struct
  include Stage_interfaces.Doubling

  let latency = Component.latency

  let create
      ~clock
      ~scope
      ({ entry_index
       ; intermediate_point =
           { point = { is_infinity; point = point_in }; requires_extra_doubling }
       ; precomputed_point_address
       ; is_handling_doubling_fault
       ; is_streaming_result
       ; is_last = _
       ; valid = valid_in
       } :
        _ Stage_input.t)
      : _ Stage_output.t
    =
    let scope = Scope.sub_scope scope "doubling_stage" in
    let spec = Reg_spec.create ~clock () in
    let { Component.O.point_out = { valid = valid_out; value = point_out }
        ; point_out_z_squared
        }
      =
      Component.hierarchical
        scope
        { clock
        ; point_in =
            { valid =
                (* Even when handling doubling fault on a point that doesn't
                 * require doubling, this needs to be vdd. The state machine
                 * checks writeback_stage.valid to be asserted to determine
                 * if the double fault handling is completed.
                 *)
                valid_in &: ~:is_streaming_result
            ; value = point_in
            }
        }
    in
    let pipe = pipeline ~n:latency spec in
    { entry_index = pipe entry_index
    ; intermediate_point =
        { point =
            { is_infinity =
                pipe is_infinity (* doubling infinity will still be infinity. *)
            ; point = point_out
            }
        ; requires_extra_doubling =
            (* A point that required doubling will never require doubling later. *)
            gnd
        }
    ; intermediate_point_z_squared = point_out_z_squared
    ; intermediate_point_write_to_buffer =
        pipe (valid_in &: is_handling_doubling_fault &: requires_extra_doubling)
    ; is_handling_doubling_fault = pipe is_handling_doubling_fault
    ; precomputed_point_address = pipe precomputed_point_address
    ; valid = valid_out
    }
  ;;
end
