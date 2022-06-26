open! Base
open! Hardcaml

module I : sig
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t = { done_ : 'a } [@@deriving sexp_of, hardcaml]
end

val create : Scope.t -> Signal.t Interface.Create_fn(I)(O).t

module Reference : sig
  module Gf : module type of Gf.Make (Hardcaml.Bits)

  val bit_reversed_addressing : 'a array -> unit
  val ntt : Gf.t array -> unit
end
