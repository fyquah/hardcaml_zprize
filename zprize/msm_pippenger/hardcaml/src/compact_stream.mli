open Hardcaml
open Hardcaml_axi

(** Takes An AXI512 stream from DDR and converts it into a 256 bit stream,
    useful for crossing SLRs when not bandwidth bound. *)

module Make (Config : Config.S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; up : 'a Axi512.Stream.Source.t
      ; dn_dest : 'a Axi256.Stream.Dest.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { dn : 'a Axi256.Stream.Source.t
      ; up_dest : 'a Axi512.Stream.Dest.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  val num_256_words_per_point : int
  val create : Scope.t -> Signal.t I.t -> Signal.t O.t
  val hierarchical : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
end
