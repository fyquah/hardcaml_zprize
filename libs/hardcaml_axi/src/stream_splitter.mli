open Hardcaml

module type Config = sig
  val num_outputs : int
end

module Make (Stream : Stream.S) (Config : Config) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; up : 'a Stream.Source.t
      ; dn_dests : 'a Stream.Dest.t array [@length num_outputs]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { dns : 'a Stream.Source.t array [@length num_outputs]
      ; up_dest : 'a Stream.Dest.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : Scope.t -> Signal.t I.t -> Signal.t O.t
end
