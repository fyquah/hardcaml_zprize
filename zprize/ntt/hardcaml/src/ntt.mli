open! Base
open! Hardcaml

type twiddle_4step_config =
  { rows_per_iteration : int
  ; log_num_iterations : int
  }
[@@deriving sexp_of]

module type Config = sig
  val logn : int
  val twiddle_4step_config : twiddle_4step_config option
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
        ; twiddle_update_in : 'a [@bits Gf.num_bits]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module Twiddle_update : sig
      type 'a t =
        { valid : 'a
        ; factors : 'a array [@bits Gf.num_bits] [@length 2]
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

    val twiddle_roots_z : row:int -> iter:int -> Gf_z.t list
    val twiddle_scale_z : Gf_z.t list
    val twiddle_omega_z : int -> Gf_z.t list
    val create : ?row:int -> Scope.t -> Signal.t Interface.Create_fn(I)(O).t
    val hierarchy : ?row:int -> Scope.t -> Signal.t Interface.Create_fn(I)(O).t
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
        ; twiddle_update : 'a Controller.Twiddle_update.t
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O : sig
      type 'a t =
        { q1 : 'a
        ; q2 : 'a
        ; twiddle_update_q : 'a
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

    val create : ?row:int -> Scope.t -> Signal.t Interface.Create_fn(I)(O).t
    val hierarchy : ?row:int -> Scope.t -> Signal.t Interface.Create_fn(I)(O).t
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
end
