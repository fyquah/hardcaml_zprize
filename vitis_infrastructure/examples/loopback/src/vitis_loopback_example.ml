open! Base
open! Hardcaml
open! Hardcaml_axi

module Axilite_control = struct
  module Config = struct
    let addr_bits = 6
    let data_bits = 32
  end

  include Lite.Make (Config)
end

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

module Wr = struct
  type 'a t =
    { foo : 'a [@bits 32]
    ; bar : 'a [@bits 32]
    }
  [@@deriving sexp_of, hardcaml]
end

module Rd = struct
  type 'a t =
    { foo_plus_one : 'a [@bits 32]
    ; bar_plus_one : 'a [@bits 32]
    }
  [@@deriving sexp_of, hardcaml]
end

module Register_bank = Axilite_control.Register_bank (Rd) (Wr)

let create
    scope
    { I.ap_clk = clock; ap_rst_n; host_to_fpga; fpga_to_host_dest; master_to_slave }
  =
  let open Signal in
  let clear = ~:ap_rst_n in
  let read_values = Rd.Of_signal.wires () in
  let { Register_bank.O.slave_to_master; write_values } =
    Register_bank.hierarchical scope { clock; clear; master_to_slave; read_values }
  in
  read_values.foo_plus_one <== write_values.foo +:. 1;
  read_values.bar_plus_one <== write_values.bar +:. 1;
  { O.fpga_to_host =
      { host_to_fpga with
        tdata =
          host_to_fpga.tdata
          |> Signal.split_lsb ~part_width:32
          |> List.map ~f:Signal.bswap
          |> Signal.concat_lsb
      }
  ; host_to_fpga_dest = fpga_to_host_dest
  ; slave_to_master
  }
;;
