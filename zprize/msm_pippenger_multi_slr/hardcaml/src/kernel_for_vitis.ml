open Core
open Hardcaml
open Hardcaml_axi
open Signal

module Make (C : Config.S) = struct
  let config = C.t
  let num_cores = Array.length config.scalar_bits_by_core

  module Kernel_for_single_instance = Kernel_for_single_instance.Make (C)

  module Stream_splitter_512 =
    Stream_splitter.Make
      (Axi512.Stream)
      (struct
        let num_outputs = num_cores
      end)

  module I = struct
    type 'a t =
      { ap_clk : 'a
      ; ap_rst_n : 'a
      ; host_to_fpga : 'a Axi512.Stream.Source.t
      ; fpga_to_host_dest : 'a Axi512.Stream.Dest.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { fpga_to_host : 'a Axi512.Stream.Source.t
      ; host_to_fpga_dest : 'a Axi512.Stream.Dest.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create ~build_mode (scope : Scope.t) (i : _ I.t) =
    let ap_clk = i.ap_clk in
    let ap_rst_n = i.ap_rst_n in
    let clock = ap_clk in
    let clear = ~:ap_rst_n in
    let host_to_fpga_dests =
      Array.init num_cores ~f:(fun _ -> Axi512.Stream.Dest.Of_signal.wires ())
    in
    let { Stream_splitter_512.O.dns = host_to_fpgas; up_dest = _ } =
      Stream_splitter_512.create
        scope
        { clock; clear; up = i.host_to_fpga; dn_dests = host_to_fpga_dests }
    in
    let _sub_kernels =
      Array.init num_cores ~f:(fun core_index ->
        Kernel_for_single_instance.hierarchical
          ~build_mode
          ~core_index
          scope
          { ap_clk
          ; ap_rst_n
          ; host_to_fpga = host_to_fpgas.(core_index)
          ; fpga_to_host_dest = { tready = gnd }
          })
    in
    O.Of_signal.of_int 0
  ;;

  let hierarchical ~build_mode scope i =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"kernel" (create ~build_mode) i
  ;;
end
