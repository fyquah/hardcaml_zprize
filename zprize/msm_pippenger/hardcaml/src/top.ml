open Base
open Hardcaml
open Signal
open Snarks_r_fun

let field_bits = 377
let scalar_bits = 253

module Ec_fpn_mixed_add = Ec_fpn_mixed_add.With_interface (struct
  let bits = field_bits
end)

let config = Config_presets.For_bls12_377.ec_fpn_ops_with_montgomery_reduction
let latency = Ec_fpn_mixed_add.latency config

module Make (Config : Pippenger.Config.S) = struct
  module Config = struct
    include Config

    let pipeline_depth = latency
    let affine_point_bits = field_bits * 2
  end

  open Config

  let log_num_windows = Int.ceil_log2 num_windows
  let jacobian_bits = field_bits * 3

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; scalar : 'a [@bits scalar_bits]
      ; scalar_valid : 'a
      ; last_scalar : 'a
      ; affine_point : 'a [@bits affine_point_bits]
      ; jacobian_point_ready : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { jacobian_point : 'a [@bits jacobian_bits]
      ; jacobian_point_valid : 'a
      ; scalar_affine_point_ready : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create
    scope
    { I.clock
    ; clear
    ; start
    ; scalar
    ; scalar_valid
    ; last_scalar
    ; affine_point
    ; jacobian_point_ready
    }
    =
    let ctrl =
      Controller.hierarchy
        scope
        { Controller.I.clock
        ; clear
        ; start
        ; scalar
        ; scalar_valid
        ; last_scalar
        ; affine_point
        }
    in
    let adder =
      Ec_fpn_mixed_add.hierarchical
        scope
        ~config
        { clock; enable = vdd; valid_in; data_in0; data_in1; data_in1_z_squared }
    in
    let port_a, port_b =
      List.init num_windows ~f:(fun window ->
        let q =
          Ram.create
            ~collision_mode:Write_before_read
            ~size:(1 lsl window_size_bits)
            ~read_ports:
              [| { read_clock = clock; read_address = ctrl.bucket; read_enable = vdd }
               ; { read_clock = clock
                 ; read_address = i.bucket_address
                 ; read_enable = i.bucket_read_enable
                 }
              |]
            ~write_ports:
              [| { write_clock = clock
                 ; write_address = dp.bucket
                 ; write_enable = dp.write_enable &: (dp.window ==:. window)
                 ; write_data = dp.result
                 }
              |]
            ()
        in
        q.(0), q.(1))
      |> List.unzip
    in
    O.Of_signal.of_int 0
  ;;

  let hierarchical ~config ~p scope i =
    let module H = Hierarchy.In_scope (I) (O) in
    Hierarchical.hierarchical ~name:"top" ~scope create
  ;;
end
