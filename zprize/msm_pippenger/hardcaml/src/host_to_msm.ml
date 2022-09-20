open Hardcaml
module Axi256 = Hardcaml_axi.Axi256

module Make (Config : Config.S) = struct
  module Compact_stream = Compact_stream.Make (Config)
  module Top = Top.Make (Config)
  module Msm_result_to_host = Msm_result_to_host.Make (Config)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; host_to_fpga : 'a Axi256.Stream.Source.t [@rtlprefix "host_to_fpga_"]
      ; fpga_to_host_dest : 'a Axi256.Stream.Dest.t [@rtlprefix "fpga_to_host_"]
      ; scalar_and_input_point_ready : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { scalar : 'a [@bits Config.scalar_bits]
      ; scalar_valid : 'a
      ; last_scalar : 'a
      ; input_point : 'a Top.Mixed_add.Xyt.t [@rtl_prefix "i_"]
      ; result_point_ready : 'a
      ; host_to_fpga_dest : 'a Axi256.Stream.Dest.t [@rtlprefix "host_to_fpga_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create _scope (_i : _ I.t) = O.Of_signal.of_int 0

  let hierarchical ?instance scope =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ?instance ~name:"host_to_msm" ~scope create
  ;;
end
