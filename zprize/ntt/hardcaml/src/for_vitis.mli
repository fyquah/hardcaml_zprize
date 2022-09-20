open! Base
open Hardcaml

module Make (Config : Hardcaml_ntt.Core_config.S) : sig
  module Four_step : module type of Hardcaml_ntt.Four_step.Make (Config)
  module Axi_stream = Four_step.Axi_stream
  module Transposer = Hardcaml_ntt.Transposer
  module Top : module type of Top.Make (Config)

  module I : sig
    type 'a t =
      { ap_clk : 'a
      ; ap_rst_n : 'a
      ; controller_to_compute_phase_1 : 'a Axi_stream.Source.t
      ; controller_to_compute_phase_2 : 'a Axi_stream.Source.t
      ; compute_to_controller_dest : 'a Axi_stream.Dest.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { compute_to_controller : 'a Axi_stream.Source.t
      ; controller_to_compute_phase_1_dest : 'a Axi_stream.Dest.t
      ; controller_to_compute_phase_2_dest : 'a Axi_stream.Dest.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : build_mode:Build_mode.t -> Scope.t -> Signal.t Interface.Create_fn(I)(O).t
end
