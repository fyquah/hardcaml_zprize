open Core
open Hardcaml
open Hardcaml_axi
open Signal

module Make (C : Config.S) = struct
  let num_cores = Array.length C.t.scalar_bits_by_core

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; sub_fpga_to_hosts : 'a Axi512.Stream.Source.t array [@length num_cores]
      ; fpga_to_host_dest : 'a Axi512.Stream.Dest.t
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:true]
  end

  module O = struct
    type 'a t =
      { fpga_to_host : 'a Axi512.Stream.Source.t
      ; sub_fpga_to_host_dests : 'a Axi512.Stream.Dest.t array [@length num_cores]
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:true]
  end

  let create (_ : Scope.t) { I.clock; clear; sub_fpga_to_hosts; fpga_to_host_dest } =
    let spec = Reg_spec.create ~clock ~clear () in
    let which_stream = wire (Int.ceil_log2 num_cores) in
    let sub_fpga_to_host_selected =
      Axi512.Stream.Source.Of_signal.mux which_stream (Array.to_list sub_fpga_to_hosts)
    in
    which_stream
    <== reg
          spec
          ~enable:
            (sub_fpga_to_host_selected.tvalid
            &: sub_fpga_to_host_selected.tlast
            &: fpga_to_host_dest.tready)
          (mux2
             (which_stream ==:. num_cores - 1)
             (zero (Int.ceil_log2 num_cores))
             (which_stream +:. 1));
    { O.fpga_to_host = sub_fpga_to_host_selected
    ; sub_fpga_to_host_dests =
        Array.init num_cores ~f:(fun core_index ->
          { Axi512.Stream.Dest.tready =
              fpga_to_host_dest.tready &: (which_stream ==:. core_index)
          })
    }
  ;;

  let hierarchical scope i =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"gather_output_stream" create i
  ;;
end
