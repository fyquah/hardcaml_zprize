open! Hardcaml
open! Hardcaml_axi

module Make (Config : Config.S) : sig
  module I : sig
    type 'a t =
      { ap_clk : 'a
      ; ap_rst_n : 'a
      ; master_to_slave : 'a Axi32.Lite.Master_to_slave.t [@rtlmangle true]
      ; precomputed_points_wr : 'a Axi256.Stream.Source.t
      ; precomputed_points_addr : 'a Axi16.Stream.Source.t
      ; result_to_host_dest : 'a Axi128.Stream.Dest.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { slave_to_master : 'a Axi32.Lite.Slave_to_master.t [@rtlmangle true]
      ; precomputed_points_wr_dest : 'a Axi256.Stream.Dest.t
      ; precomputed_points_addr_dest : 'a Axi16.Stream.Dest.t
      ; result_to_host : 'a Axi128.Stream.Source.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : build_mode:Build_mode.t -> Scope.t -> Signal.t I.t -> Signal.t O.t
end
