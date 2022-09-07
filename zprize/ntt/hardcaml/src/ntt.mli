open! Base
open! Hardcaml

module type Config = sig
  val logn : int
  val support_4step_twiddle : bool
end

module Gf : module type of Gf_bits.Make (Signal)

module Make (Config : Config) : sig
  val n : int
  val logn : int

  module Twiddle_factor_stream : sig
    val pipe_length : int

    module I : sig
      type 'a t =
        { clock : 'a
        ; start_twiddles : 'a
        ; omegas : 'a list
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O : sig
      type 'a t = { w : 'a } [@@deriving sexp_of, hardcaml]
    end

    val create : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
    val hierarchy : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
  end

  module Controller : sig
    module I : sig
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; start : 'a
        ; first_4step_pass : 'a
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
        ; read_write_enable : 'a
        ; flip : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    val create : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
    val hierarchy : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
  end

  module Datapath : sig
    module I : sig
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; d1 : 'a
        ; d2 : 'a
        ; omegas : 'a list
        ; start_twiddles : 'a
        ; twiddle_stage : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O : sig
      type 'a t =
        { q1 : 'a
        ; q2 : 'a
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

    val create : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
    val hierarchy : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
  end

  module With_rams : sig
    module I : sig
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; start : 'a
        ; first_4step_pass : 'a
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

    val create
      :  build_mode:Build_mode.t
      -> Scope.t
      -> Signal.t Interface.Create_fn(I)(O).t

    val hierarchy
      :  ?instance:string
      -> build_mode:Build_mode.t
      -> Scope.t
      -> Signal.t Interface.Create_fn(I)(O).t
  end
end
