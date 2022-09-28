(** NTT transform controller.

    Initially we read from the input RAM which is indicated through
   [first_stage]. We then iterate by reading and writing to the tranpose RAMs.
   On [last_stage] we write to the output RAM.

   Additionally, the optional twiddle stage is performed. This performs an extra
   pass through the coefficients neccessary to implement the NTT 4 step
   algorithm.

   The controller flushes the pipeline after each ntt stage, and flips the
   [Bram]s.

   After each pass of the NTT, the controller allows the twiddle factors to be
   updated by the multipliers in the datapath. *)

open! Base
open! Hardcaml

module Make (Config : Core_config.S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a (** Start running the NTT *)
      ; first_iter : 'a
      ; first_4step_pass : 'a
          (** We are performing the 1st stage of the NTT calculation. *)
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { done_ : 'a
      ; i : 'a
      ; j : 'a
      ; k : 'a
      ; m : 'a
      ; addr1 : 'a
      ; addr2 : 'a
      ; omegas : 'a list
      ; start_twiddles : 'a
      ; first_stage : 'a
      ; last_stage : 'a
      ; twiddle_stage : 'a
      ; twiddle_update : 'a Twiddle_update.t
      ; read_write_enable : 'a
      ; flip : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
  val hierarchy : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
end
