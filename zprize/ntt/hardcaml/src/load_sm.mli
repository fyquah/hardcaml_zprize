(** Sequence the loading of coefficients from DRAM into the parallel MTT cores.

    Inputs data into all parallel NTT cores. Runs multiple times during a pass.
    The ordering of data differs depending on the pass. *)

open Hardcaml

module Make (Config : Top_config.S) : sig
  val write_pipelining : int

  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; first_4step_pass : 'a (** Loading data for the first or second pass. *)
      ; tvalid : 'a (** Input data is valid *)
      ; start : 'a (** Start inputting a block of data. *)
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { done_ : 'a (** Low while streaming. *)
      ; tready : 'a (** Read input data from AXI stream. *)
      ; wr_addr : 'a (** Write address to internal RAM. *)
      ; wr_en : 'a (** Write enable to internal RAM. *)
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : Scope.t -> Hardcaml.Signal.t Hardcaml.Interface.Create_fn(I)(O).t
  val hierarchy : Scope.t -> Hardcaml.Signal.t Hardcaml.Interface.Create_fn(I)(O).t
end
