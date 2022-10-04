(** Instantiate the parallel NTT cores along with the load and store state machines.

    Exposes AXI streaming interfaces. *)

open! Base
open Hardcaml

module Make (Config : Top_config.S) : sig
  module Four_step : module type of Hardcaml_ntt.Four_step.Make (Config)
  module Axi_stream = Four_step.Axi_stream
  module Gf = Four_step.Gf
  module Load_sm : module type of Load_sm.Make (Config)
  module Store_sm : module type of Store_sm.Make (Config)

  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a (** Begin processing a pass. *)
      ; first_4step_pass : 'a
          (** If high we are performing the first pass of the 4 step algorithm. The
              core will apply the twiddle correction factors and also stream in/out
              data in the appropriate way. *)
      ; data_in : 'a Axi_stream.Source.t
      ; data_out_dest : 'a Axi_stream.Dest.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { data_out : 'a Axi_stream.Source.t
      ; data_in_dest : 'a Axi_stream.Dest.t
      ; done_ : 'a (** Low while a pass is running. *)
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : build_mode:Build_mode.t -> Scope.t -> Signal.t Interface.Create_fn(I)(O).t

  val hierarchy
    :  build_mode:Build_mode.t
    -> Scope.t
    -> Signal.t Interface.Create_fn(I)(O).t
end
