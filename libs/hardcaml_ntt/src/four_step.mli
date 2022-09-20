(** Multipass NTT algorithm using the 4 step method. *)

open! Base
open Hardcaml

module Make (Config : Core_config.S) : sig
  val logcores : int

  module Gf = Gf.Signal
  module Axi_stream : Hardcaml_axi.Stream.S
  module Multi_parallel_cores : module type of Multi_parallel_cores.Make (Config)

  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; first_4step_pass : 'a
      ; wr_d : 'a Multi_parallel_cores.Q2d.t
      ; wr_en : 'a
      ; wr_addr : 'a array
      ; rd_en : 'a
      ; rd_addr : 'a array
      ; input_done : 'a
      ; output_done : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { done_ : 'a
      ; start_input : 'a
      ; start_output : 'a
      ; rd_q : 'a Multi_parallel_cores.Q2d.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : build_mode:Build_mode.t -> Scope.t -> Signal.t Interface.Create_fn(I)(O).t

  val hierarchy
    :  build_mode:Build_mode.t
    -> Scope.t
    -> Signal.t Interface.Create_fn(I)(O).t
end
