open Hardcaml
open Hardcaml_axi
module Write_port_1d = Hardcaml_xilinx.Memory_builder.Write_port_1d

module Make (Config : Config.S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; stream : 'a Axi256.Stream.Source.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t = { precomputed_points_wr : 'a Write_port_1d.M(Affine_point_or_infinity).t }
    [@@deriving sexp_of, hardcaml]
  end

  val create : Scope.t -> Signal.t I.t -> Signal.t O.t
  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end
