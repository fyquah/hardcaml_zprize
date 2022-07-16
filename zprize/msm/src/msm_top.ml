open Base
open Hardcaml
open Hardcaml_axi
open Signal

let max_num_bits = 377

module Make (Config : Config.S) = struct
  module I = struct
    type 'a t =
      { ap_clk : 'a
      ; ap_rst_n : 'a
      ; master_to_slave : 'a Axi32.Lite.Master_to_slave.t [@rtlprefix "s_axi_control_"]
      ; precomputed_points_wr : 'a Axi256.Stream.Source.t [@rtlmangle true]
      ; precomputed_points_addr : 'a Axi16.Stream.Source.t [@rtlmangle true]
      ; result_to_host_dest : 'a Axi128.Stream.Dest.t [@rtlprefix "result_to_host_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { slave_to_master : 'a Axi32.Lite.Slave_to_master.t [@rtlprefix "slave_to_master_"]
      ; precomputed_points_wr_dest : 'a Axi256.Stream.Dest.t
            [@rtlprefix "precomputed_points_write_"]
      ; precomputed_points_addr_dest : 'a Axi16.Stream.Dest.t
            [@rtlprefix "precomputed_points_addr_"]
      ; result_to_host : 'a Axi128.Stream.Source.t [@rtlprefix "result_to_host_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Msm_datapath = Msm_datapath.Make (Config)
  module Decode_precomputed_points_stream = Decode_precomputed_points_stream.Make (Config)
  module Encode_result = Encode_result.Make (Config)
  module Register_bank = Register_interface.Register_bank

  let create
      ~build_mode
      scope
      { I.ap_clk = clock
      ; master_to_slave
      ; ap_rst_n
      ; precomputed_points_wr
      ; precomputed_points_addr
      ; result_to_host_dest
      }
    =
    let clear = ~:ap_rst_n in
    let { Decode_precomputed_points_stream.O.precomputed_points_wr } =
      Decode_precomputed_points_stream.hierarchical
        scope
        { clock; clear; stream = precomputed_points_wr }
    in
    let { Register_bank.O.write_values = { num_entries_minus_1; scalar_num_bits_minus_1 }
        ; slave_to_master
        }
      =
      Register_bank.hierarchical
        scope
        { clock
        ; clear
        ; master_to_slave
        ; read_values =
            { datapath_max_num_entries = of_int ~width:32 Config.datapath_num_entries
            ; precomputed_points_table_size =
                of_int ~width:32 Config.precomputed_points_table_size
            }
        }
    in
    let num_entries_minus_1 =
      sel_bottom num_entries_minus_1 (Int.ceil_log2 Config.datapath_num_entries)
    in
    let scalar_num_bits_minus_1 =
      sel_bottom scalar_num_bits_minus_1 (Int.ceil_log2 max_num_bits)
    in
    let result_ready = wire 1 in
    let { Msm_datapath.O.precomputed_points_addr_dest; result; result_last } =
      Msm_datapath.hierarchical
        ~build_mode
        scope
        { clock
        ; clear
        ; precomputed_points_wr
        ; num_entries_minus_1
        ; scalar_num_bits_minus_1
        ; precomputed_points_addr
        ; result_ready
        }
    in
    let { Encode_result.O.result_to_host; result_ready = result_ready' } =
      Encode_result.hierarchical
        scope
        { clock; clear; result; result_last; result_to_host_dest }
    in
    result_ready <== result_ready';
    { O.precomputed_points_wr_dest = { tready = vdd }
    ; precomputed_points_addr_dest
    ; result_to_host
    ; slave_to_master
    }
  ;;
end
