(** Multipass NTT algorithm using the 4 step method.**)

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

  module Kernel : sig
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

    module Store_sm : sig
      module State : sig
        type t [@@deriving sexp_of, compare, enumerate]
      end

      type 'a t =
        { done_ : 'a
        ; tvalid : 'a
        ; rd_addr : 'a
        ; rd_en : 'a
        }
      [@@deriving sexp_of, hardcaml]

      val create : Signal.t I.t -> start:Signal.t -> Signal.t t
    end

    module Load_sm : sig
      module State : sig
        type t [@@deriving sexp_of, compare, enumerate]
      end

      type 'a t =
        { done_ : 'a
        ; tready : 'a
        ; wr_addr : 'a
        ; wr_en : 'a
        }
      [@@deriving sexp_of, hardcaml]

      val create : Signal.t I.t -> start:Signal.t -> Signal.t t
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

  module Kernel_for_vitis : sig
    module I : sig
      type 'a t =
        { ap_clk : 'a
        ; ap_rst_n : 'a
        ; controller_to_compute : 'a Axi_stream.Source.t
        ; compute_to_controller_dest : 'a Axi_stream.Dest.t
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O : sig
      type 'a t =
        { compute_to_controller : 'a Axi_stream.Source.t
        ; controller_to_compute_dest : 'a Axi_stream.Dest.t
        }
      [@@deriving sexp_of, hardcaml]
    end

    val create
      :  build_mode:Build_mode.t
      -> Scope.t
      -> Signal.t Interface.Create_fn(I)(O).t
  end
end
