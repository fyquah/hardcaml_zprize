(** Instantiate a single NTT core design with the associated input/output and internal RAMs. *)

open! Base
open! Hardcaml

module Make (Config : Core_config.S) : sig
  val n : int
  val logn : int

  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; first_iter : 'a
      ; first_4step_pass : 'a
      ; d1 : 'a
      ; d2 : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { q1 : 'a
      ; q2 : 'a
      ; addr1_in : 'a
      ; addr2_in : 'a
      ; read_enable_in : 'a
      ; addr1_out : 'a
      ; addr2_out : 'a
      ; write_enable_out : 'a
      ; first_stage : 'a
      ; last_stage : 'a
      ; twiddle_stage : 'a
      ; flip : 'a
      ; done_ : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : ?row:int -> Scope.t -> Signal.t Interface.Create_fn(I)(O).t
  val hierarchy : ?row:int -> Scope.t -> Signal.t Interface.Create_fn(I)(O).t
end

module With_rams (Config : Core_config.S) : sig
  module Core : module type of Make (Config)

  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; first_4step_pass : 'a
      ; first_iter : 'a
      ; flip : 'a
      ; wr_d : 'a
      ; wr_en : 'a
      ; wr_addr : 'a
      ; rd_en : 'a
      ; rd_addr : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { done_ : 'a
      ; rd_q : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val input_ram
    :  Scope.t
    -> Build_mode.t
    -> clock:Signal.t
    -> clear:Signal.t
    -> flip:Signal.t
    -> wr_addr:Signal.t
    -> wr_d:Signal.t
    -> wr_en:Signal.t
    -> addr1_in:Signal.t
    -> addr2_in:Signal.t
    -> read_enable_in:Signal.t
    -> Signal.t * Signal.t

  val transpose_ram
    :  Scope.t
    -> Build_mode.t
    -> clock:Signal.t
    -> clear:Signal.t
    -> addr1_in:Signal.t
    -> addr1_out:Signal.t
    -> q1:Signal.t
    -> addr2_in:Signal.t
    -> addr2_out:Signal.t
    -> q2:Signal.t
    -> read_enable_in:Signal.t
    -> write_enable_out:Signal.t
    -> flip:Signal.t
    -> last_stage:Signal.t
    -> Signal.t * Signal.t

  val output_ram
    :  Scope.t
    -> Build_mode.t
    -> clock:Signal.t
    -> clear:Signal.t
    -> flip:Signal.t
    -> last_stage:Signal.t
    -> twiddle_stage:Signal.t
    -> rd_addr:Signal.t
    -> rd_en:Signal.t
    -> addr1_out:Signal.t
    -> q1:Signal.t
    -> addr2_out:Signal.t
    -> q2:Signal.t
    -> write_enable_out:Signal.t
    -> Signal.t

  val create
    :  ?row:int
    -> build_mode:Build_mode.t
    -> Scope.t
    -> Signal.t Interface.Create_fn(I)(O).t

  val hierarchy
    :  ?row:int
    -> ?instance:string
    -> build_mode:Build_mode.t
    -> Scope.t
    -> Signal.t Interface.Create_fn(I)(O).t
end
