open Hardcaml
open Hardcaml_axi

module Make (C : Config.S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; sub_fpga_to_hosts : 'a Axi512.Stream.Source.t array
      ; fpga_to_host_dest : 'a Axi512.Stream.Dest.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { fpga_to_host : 'a Axi512.Stream.Source.t
      ; sub_fpga_to_host_dests : 'a Axi512.Stream.Dest.t array
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : Scope.t -> Signal.t I.t -> Signal.t O.t
  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end
