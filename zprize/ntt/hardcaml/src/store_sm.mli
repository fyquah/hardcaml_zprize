(** Sequence the storing of coefficients from the parallel MTT cores into DRAM.

    Outputs data from all parallel NTT cores.

    Runs multiple times during a pass.
*)

open Hardcaml

module Make (Config : Top_config.S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; first_4step_pass : 'a (** Storing data for the first or second pass. *)
      ; tready : 'a (** Output AXI stream can accept data. *)
      ; start : 'a (** Start outputting a block of data. *)
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { done_ : 'a (** Low while streaming. *)
      ; tvalid : 'a (** Output data is valid. *)
      ; rd_addr : 'a (** Read address to internal RAM. *)
      ; rd_en : 'a (** Read enable to internal RAM. *)
      ; rd_any : 'a (** Any one of the RAMs is being accessed *)
      ; block : 'a
          (** Internal block of NTTs being accessed (forms part of read address). *)
      }
    [@@deriving sexp_of, hardcaml]
  end

  val read_address_pipelining : int
  val read_data_pipelining : int
  val read_data_tree_mux_stages : int
  val create : Scope.t -> Hardcaml.Signal.t Hardcaml.Interface.Create_fn(I)(O).t
  val hierarchy : Scope.t -> Hardcaml.Signal.t Hardcaml.Interface.Create_fn(I)(O).t
end
