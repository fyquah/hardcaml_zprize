open Base
open Hardcaml

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
val initial_pipeline_factors : int -> Signal.t list
