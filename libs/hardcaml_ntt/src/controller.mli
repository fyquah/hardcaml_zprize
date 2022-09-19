open! Base
open! Hardcaml

module Make (Config : Core_config.S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; first_iter : 'a
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
      ; twiddle_update : 'a Twiddle_update.t
      ; read_write_enable : 'a
      ; flip : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
  val hierarchy : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
end
