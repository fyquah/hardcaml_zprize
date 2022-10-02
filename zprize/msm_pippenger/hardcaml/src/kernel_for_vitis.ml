open Hardcaml
open Signal
module Axi512 = Hardcaml_axi.Axi512
module Axi256 = Hardcaml_axi.Axi256

module Make (Config : Config.S) = struct
  module Compact_stream = Compact_stream.Make (Config)
  module Top = Top.Make (Config)
  module Msm_result_to_host = Msm_result_to_host.Make (Config)
  module Host_to_msm_simple = Host_to_msm_simple.Make (Config)
  module Merge_axi_streams = Merge_axi_streams.Make (Config)
  module Scalar_transformation = Scalar_transformation.Make (Config)

  module I = struct
    type 'a t =
      { ap_clk : 'a
      ; ap_rst_n : 'a
      ; host_scalars_to_fpga : 'a Axi512.Stream.Source.t
           [@rtlprefix "host_scalars_to_fpga_"]
      ; ddr_points_to_fpga : 'a Axi512.Stream.Source.t [@rtlprefix "ddr_points_to_fpga_"]
      ; fpga_to_host_dest : 'a Axi512.Stream.Dest.t [@rtlprefix "fpga_to_host_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { fpga_to_host : 'a Axi512.Stream.Source.t [@rtlprefix "fpga_to_host_"]
      ; host_scalars_to_fpga_dest : 'a Axi512.Stream.Dest.t
           [@rtlprefix "host_scalars_to_fpga_"]
      ; ddr_points_to_fpga_dest : 'a Axi512.Stream.Dest.t
           [@rtlprefix "ddr_points_to_fpga_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module State = struct
    type t =
      | Idle
      | Working
    [@@deriving sexp_of, compare, enumerate]
  end

  let create
    ~build_mode
    scope
    { I.ap_clk; ap_rst_n; host_scalars_to_fpga; ddr_points_to_fpga; fpga_to_host_dest }
    =
    let clock = ap_clk in
    let clear = ~:ap_rst_n in
    (* transform the scalars off the wire *)
    let transformed_scalars_to_fpga_dest = Axi512.Stream.Dest.Of_signal.wires () in
    let { Scalar_transformation.O.transformed_scalars_to_fpga = host_scalars_to_fpga
        ; host_scalars_to_fpga_dest
        }
      =
      Scalar_transformation.hierarchical
        scope
        { clock; clear; host_scalars_to_fpga; transformed_scalars_to_fpga_dest }
    in
    (* merge the axi streams *)
    let host_to_fpga_dest = Axi512.Stream.Dest.Of_signal.wires () in
    let { Merge_axi_streams.O.host_to_fpga
        ; host_scalars_to_fpga_dest = transformed_scalars_to_fpga_dest'
        ; ddr_points_to_fpga_dest
        }
      =
      Merge_axi_streams.hierarchical
        scope
        { clock; clear; host_scalars_to_fpga; ddr_points_to_fpga; host_to_fpga_dest }
    in
    Axi512.Stream.Dest.Of_signal.( <== )
      transformed_scalars_to_fpga_dest
      transformed_scalars_to_fpga_dest';
    let scalar_and_input_point_ready = wire 1 in
    let host_to_msm =
      Host_to_msm_simple.hierarchical
        scope
        { clock; clear; host_to_fpga; scalar_and_input_point_ready }
    in
    Axi512.Stream.Dest.Of_signal.( <== ) host_to_fpga_dest host_to_msm.host_to_fpga_dest;
    let result_point_ready = wire 1 in
    let top =
      Top.hierarchical
        ~build_mode
        scope
        { clock
        ; clear
        ; scalar = host_to_msm.scalar
        ; input_point = host_to_msm.input_point
        ; scalar_valid = host_to_msm.scalar_valid
        ; last_scalar = host_to_msm.last_scalar
        ; result_point_ready
        }
    in
    scalar_and_input_point_ready <== top.scalar_and_input_point_ready;
    let msm_result_to_host =
      Msm_result_to_host.hierarchical
        scope
        ~drop_t:gnd
        { clock
        ; clear
        ; result_point =
            { x = top.result_point.x
            ; y = top.result_point.y
            ; z = top.result_point.z
            ; t = top.result_point.t
            }
        ; result_point_valid = top.result_point_valid
        ; last_result_point = top.last_result_point
        ; fpga_to_host_dest
        }
    in
    result_point_ready <== msm_result_to_host.result_point_ready;
    { O.host_scalars_to_fpga_dest
    ; ddr_points_to_fpga_dest
    ; fpga_to_host = msm_result_to_host.fpga_to_host
    }
  ;;

  let hierarchical ~build_mode scope =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~name:"kernel_for_vitis" ~scope (create ~build_mode)
  ;;
end
