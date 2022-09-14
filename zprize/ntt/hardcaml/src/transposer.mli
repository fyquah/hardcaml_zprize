open Hardcaml
module Axi512 = Hardcaml_axi.Axi512

val transposer_height : int

module I : sig
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; transposer_in : 'a Axi512.Stream.Source.t
    ; transposer_out_dest : 'a Axi512.Stream.Dest.t
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t =
    { transposer_out : 'a Axi512.Stream.Source.t
    ; transposer_in_dest : 'a Axi512.Stream.Dest.t
    }
  [@@deriving sexp_of, hardcaml]
end

val create : transposer_depth_in_cycles:int -> Scope.t -> Signal.t I.t -> Signal.t O.t

val hierarchical
  :  transposer_depth_in_cycles:int
  -> Scope.t
  -> Signal.t I.t
  -> Signal.t O.t
