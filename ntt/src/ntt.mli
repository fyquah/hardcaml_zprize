open! Base
open! Hardcaml
module Gfhw : module type of Gf.Make (Hardcaml.Signal)

module Address_controller : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
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
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
end

module Reference : sig
  module Gf : module type of Gf.Make (Hardcaml.Bits)

  val bit_reversed_addressing : 'a array -> unit
  val ntt : Gf.t array -> unit
end
