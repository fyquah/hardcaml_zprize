open! Base
open Hardcaml

module I : sig
  type 'a t =
    { clock : 'a
    ; clear : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t = { w : 'a [@bits Gf.num_bits] } [@@deriving sexp_of, hardcaml]
end

val create : logn:int -> size:int -> Scope.t -> Signal.t Interface.Create_fn(I)(O).t
