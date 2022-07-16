open Hardcaml
open Hardcaml_axi

module Make (Config : Config.S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; result : 'a Jacobian_point_or_infinity.With_valid.t
      ; result_last : 'a
      ; result_to_host_dest : 'a Axi128.Stream.Dest.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { result_to_host : 'a Axi128.Stream.Source.t
      ; result_ready : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : Scope.t -> Signal.t I.t -> Signal.t O.t
  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end
