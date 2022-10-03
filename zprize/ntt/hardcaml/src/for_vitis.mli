(** Top-level wrapper with port naming to suit the Vivido Vitis infrastructure.

    Directly used as a Vitis RTL kernel.

    Instantiates the top level NTT design, plus the transposer unit used in the 2nd pass.
*)

open! Base
open Hardcaml

module Make (Config : Top_config.S) : sig
  module Four_step : module type of Hardcaml_ntt.Four_step.Make (Config)
  module Axi_stream = Four_step.Axi_stream

  module I : sig
    type 'a t =
      { ap_clk : 'a
      ; ap_rst_n : 'a (** Active low reset *)
      ; controller_to_compute_phase_1 : 'a Axi_stream.Source.t
          (** Data streamed in for pass 1 *)
      ; controller_to_compute_phase_2 : 'a Axi_stream.Source.t
          (** Data streamed in for pass 2 *)
      ; compute_to_controller_dest : 'a Axi_stream.Dest.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { compute_to_controller : 'a Axi_stream.Source.t (** Data streamed out. *)
      ; controller_to_compute_phase_1_dest : 'a Axi_stream.Dest.t
      ; controller_to_compute_phase_2_dest : 'a Axi_stream.Dest.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : build_mode:Build_mode.t -> Scope.t -> Signal.t Interface.Create_fn(I)(O).t
end
