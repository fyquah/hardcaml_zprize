open Hardcaml
open Hardcaml_axi

module Axilite_control : sig
  module Config : Config
  include module type of Lite.Make (Config)
end

module I : sig
  type 'a t =
    { ap_clk : 'a
    ; ap_rst_n : 'a
    ; host_to_fpga : 'a Axi512.Stream.Source.t
    ; fpga_to_host_dest : 'a Axi512.Stream.Dest.t
    ; master_to_slave : 'a Axilite_control.Master_to_slave.t
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t =
    { fpga_to_host : 'a Axi512.Stream.Source.t
    ; host_to_fpga_dest : 'a Axi512.Stream.Dest.t
    ; slave_to_master : 'a Axilite_control.Slave_to_master.t
    }
  [@@deriving sexp_of, hardcaml]
end

val create : Scope.t -> Signal.t I.t -> Signal.t O.t
