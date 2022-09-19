open! Base
open Hardcaml

module Make (Config : Hardcaml_ntt.Four_step_config.S) : sig
  module Ntt_4step : module type of Hardcaml_ntt.Ntt_4step.Make (Config)
  module Axi_stream = Ntt_4step.Axi_stream
  module Gf = Ntt_4step.Gf
  module Load_sm : module type of Load_sm.Make (Config)
  module Store_sm : module type of Store_sm.Make (Config)

  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; first_4step_pass : 'a
      ; data_in : 'a Axi_stream.Source.t
      ; data_out_dest : 'a Axi_stream.Dest.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { data_out : 'a Axi_stream.Source.t
      ; data_in_dest : 'a Axi_stream.Dest.t
      ; done_ : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : build_mode:Build_mode.t -> Scope.t -> Signal.t Interface.Create_fn(I)(O).t

  val hierarchy
    :  build_mode:Build_mode.t
    -> Scope.t
    -> Signal.t Interface.Create_fn(I)(O).t
end
