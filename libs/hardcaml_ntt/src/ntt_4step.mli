(** Multipass NTT algorithm using the 4 step method. *)

open! Base
open Hardcaml

module type Config = sig
  include Ntt.Config

  val logcores : int
end

module Make (Config : Config) : sig
  val logcores : int

  module Gf : module type of Gf_bits.Make (Hardcaml.Signal)
  module Axi_stream : Hardcaml_axi.Stream.S

  module Parallel_cores : sig
    module I : sig
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; start : 'a
        ; first_4step_pass : 'a
        ; first_iter : 'a
        ; flip : 'a
        ; wr_d : 'a array
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
        ; rd_q : 'a array
        }
      [@@deriving sexp_of, hardcaml]
    end

    val create
      :  build_mode:Build_mode.t
      -> Scope.t
      -> Signal.t Interface.Create_fn(I)(O).t

    val hierarchy
      :  build_mode:Build_mode.t
      -> Scope.t
      -> Signal.t Interface.Create_fn(I)(O).t
  end

  module Controller : sig
    module I : sig
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; start : 'a
        ; input_done : 'a
        ; output_done : 'a
        ; cores_done : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O : sig
      type 'a t =
        { done_ : 'a
        ; start_input : 'a
        ; start_output : 'a
        ; start_cores : 'a
        ; first_iter : 'a
        ; flip : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    val create : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
    val hierarchy : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
  end

  module Core : sig
    module I : sig
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; start : 'a
        ; first_4step_pass : 'a
        ; wr_d : 'a array
        ; wr_en : 'a
        ; wr_addr : 'a
        ; rd_en : 'a
        ; rd_addr : 'a
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
        ; rd_q : 'a array
        }
      [@@deriving sexp_of, hardcaml]
    end

    val create
      :  build_mode:Build_mode.t
      -> Scope.t
      -> Signal.t Interface.Create_fn(I)(O).t

    val hierarchy
      :  build_mode:Build_mode.t
      -> Scope.t
      -> Signal.t Interface.Create_fn(I)(O).t
  end
end
