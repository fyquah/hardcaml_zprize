(** Wraps the MSM pippenger top module with 512 bit AXI stream interfaces that
    can be driven by the AWS shell when compiled with Vitis.
*)

open Hardcaml
open Hardcaml_axi

module I : sig
  type 'a t =
    { ap_clk : 'a
    ; ap_rst_n : 'a
    ; host_to_fpga : 'a Axi512.Stream.Source.t
    ; fpga_to_host_dest : 'a Axi512.Stream.Dest.t
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t =
    { fpga_to_host : 'a Axi512.Stream.Source.t
    ; host_to_fpga_dest : 'a Axi512.Stream.Dest.t
    }
  [@@deriving sexp_of, hardcaml]
end

val create : build_mode:Build_mode.t -> Scope.t -> Signal.t I.t -> Signal.t O.t
