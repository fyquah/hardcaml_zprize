open Hardcaml

module Make (Config : Config.S) (Num_bits : Twisted_edwards_lib.Num_bits.S) : sig
  module Xyt : module type of Twisted_edwards_lib.Xyt.Make (Num_bits)

  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; scalar : 'a
      ; scalar_valid : 'a
      ; last_scalar : 'a
      ; input_point : 'a Xyt.t
      ; scalar_and_input_point_ready : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { scalar : 'a array
      ; scalar_negatives : 'a array
      ; scalar_valid : 'a
      ; last_scalar : 'a
      ; input_point : 'a Xyt.t
      ; scalar_and_input_point_ready : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : Scope.t -> Signal.t I.t -> Signal.t O.t
  val hierarchical : ?instance:string -> Scope.t -> Signal.t Interface.Create_fn(I)(O).t
end
