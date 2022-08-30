open Hardcaml
open Signal
open Elliptic_curve_lib

include struct
  open Elliptic_curve_lib
  module Ec_fpn_mixed_add = Ec_fpn_mixed_add
end

module Ec_fpn_mixed_add_377 = Ec_fpn_mixed_add.With_interface (struct
  let bits = 377
end)

module Component = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; intermediate_point : 'a Jacobian_point_with_metadata.t
            [@rtlprefix "intermediate$"]
      ; intermediate_point_z_squared : 'a [@bits 377] [@rtlname "intermediate$z_squared"]
      ; precomputed_point : 'a Affine_point_or_infinity.t [@rtlprefix "precomputed$"]
      ; valid_in : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { point_out : 'a Jacobian_point_with_metadata.With_valid.t [@rtlmangle true] }
    [@@deriving sexp_of, hardcaml]
  end

  let config = Config_presets.For_bls12_377.ec_fpn_ops_with_montgomery_reduction
  let latency = Ec_fpn_mixed_add_377.latency config

  let create
      scope
      { I.clock
      ; intermediate_point
      ; intermediate_point_z_squared
      ; precomputed_point
      ; valid_in
      }
      : _ O.t
    =
    let pipe x = Signal.pipeline ~n:latency (Reg_spec.create ~clock ()) ~enable:vdd x in
    let { Ec_fpn_mixed_add_377.O.valid_out; ready_in = _; data_out; error } =
      Ec_fpn_mixed_add_377.create
        ~config
        scope
        { clock
        ; enable = vdd
        ; valid_in
        ; data_in0 = precomputed_point.point
        ; data_in1 = intermediate_point.point.point
        ; data_in1_z_squared = intermediate_point_z_squared
        }
    in
    { O.point_out =
        { valid = valid_out
        ; value =
            { requires_extra_doubling =
                error
                &: valid_out
                &: pipe
                     ~:(precomputed_point.is_infinity
                       |: intermediate_point.point.is_infinity)
            ; point =
                { is_infinity =
                    pipe
                      (intermediate_point.point.is_infinity
                      &: precomputed_point.is_infinity)
                ; point =
                    Jacobian_point.Of_signal.mux2
                      (pipe intermediate_point.point.is_infinity)
                      (Jacobian_point.map
                         ~f:pipe
                         (Jacobian_point.of_affine_point precomputed_point.point))
                    @@ Jacobian_point.Of_signal.mux2
                         (error |: pipe precomputed_point.is_infinity)
                         (Jacobian_point.map ~f:pipe intermediate_point.point.point)
                    @@ data_out
                }
            }
        }
    }
  ;;

  let hierarchical scope i =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~name:"adding_stage" ~scope create i
  ;;
end

module Make (Config : Config.S) (Stage_interfaces : Stage_interfaces.M(Config).S) = struct
  include Stage_interfaces.Adding

  let latency = Component.latency

  let create
      ~clock
      ~scope
      { Stage_input.precomputed_point
      ; intermediate_point
      ; intermediate_point_z_squared
      ; entry_index
      ; valid
      }
      : _ Stage_output.t
    =
    let spec = Reg_spec.create ~clock () in
    let pipe = pipeline ~n:latency spec in
    let { Component.O.point_out = { valid = valid_out; value = intermediate_point } } =
      Component.hierarchical
        scope
        { clock
        ; intermediate_point
        ; intermediate_point_z_squared
        ; precomputed_point
        ; valid_in = valid
        }
    in
    { Stage_output.valid = valid_out; entry_index = pipe entry_index; intermediate_point }
  ;;
end
