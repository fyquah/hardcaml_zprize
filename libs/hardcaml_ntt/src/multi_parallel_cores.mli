(** Instantiate [2^Config.logblocks] parallel cores.

    Gives a total of [2^(Config.logblocks + Config.logcores)] ntt cores.
*)

open! Base
open! Hardcaml

module Make (Config : Core_config.S) : sig
  (** 2D array of [Gf] values.  Indexed by [a.(block).(core)] *)
  module Q2d : Hardcaml.Interface.S with type 'a t = 'a array array

  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; first_4step_pass : 'a
      ; first_iter : 'a
      ; flip : 'a
      ; wr_d : 'a Q2d.t
      ; wr_en : 'a
      ; wr_addr : 'a array
      ; rd_en : 'a
      ; rd_addr : 'a array
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { done_ : 'a
      ; rd_q : 'a Q2d.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : build_mode:Build_mode.t -> Scope.t -> Signal.t Interface.Create_fn(I)(O).t

  val hierarchy
    :  build_mode:Build_mode.t
    -> Scope.t
    -> Signal.t Interface.Create_fn(I)(O).t
end
