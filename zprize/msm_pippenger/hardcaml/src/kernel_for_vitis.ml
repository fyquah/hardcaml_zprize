open Hardcaml_axi

module Axilite_control = struct
  module Config = struct
    let addr_bits = 6
    let data_bits = 32
  end

  include Lite.Make (Config)
end

module Axi512 = Hardcaml_axi.Axi512

module I = struct
  type 'a t =
    { ap_clk : 'a
    ; ap_rst_n : 'a
    ; host_to_fpga : 'a Axi512.Stream.Source.t [@rtlprefix "host_to_fpga_"]
    ; fpga_to_host_dest : 'a Axi512.Stream.Dest.t [@rtlprefix "fpga_to_host_"]
    ; master_to_slave : 'a Axilite_control.Master_to_slave.t [@rtlprefix "s_axi_control_"]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { fpga_to_host : 'a Axi512.Stream.Source.t [@rtlprefix "fpga_to_host_"]
    ; host_to_fpga_dest : 'a Axi512.Stream.Dest.t [@rtlprefix "host_to_fpga_"]
    ; slave_to_master : 'a Axilite_control.Slave_to_master.t [@rtlprefix "s_axi_control_"]
    }
  [@@deriving sexp_of, hardcaml]
end

let create
  ~build_mode:_
  _scope
  { I.ap_clk = _
  ; ap_rst_n = _
  ; host_to_fpga = _
  ; fpga_to_host_dest = _
  ; master_to_slave = _
  }
  =
  O.Of_signal.of_int 0
;;
