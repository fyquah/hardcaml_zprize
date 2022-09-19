open! Base
open Hardcaml

module Make (Config : Four_step_config.S) : sig
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

  val create : build_mode:Build_mode.t -> Scope.t -> Signal.t Interface.Create_fn(I)(O).t

  val hierarchy
    :  build_mode:Build_mode.t
    -> Scope.t
    -> Signal.t Interface.Create_fn(I)(O).t
end