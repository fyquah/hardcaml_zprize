(** A pipeline that transforms scalars into the form that is expected on the
    FPGA. The main transform used currently is converting scalars from the range
    [1, 2^{c}-1] into the range [-2^{c-1}, 2^{c-1}-1] which allows for mapping point 
    addition in negative buckets to point subtraction in positive buckets *)

open Hardcaml
module Axi512 = Hardcaml_axi.Axi512

module Make (Config : Config.S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; host_scalars_to_fpga : 'a Axi512.Stream.Source.t
      ; transformed_scalars_to_fpga_dest : 'a Axi512.Stream.Dest.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { transformed_scalars_to_fpga : 'a Axi512.Stream.Source.t
      ; host_scalars_to_fpga_dest : 'a Axi512.Stream.Dest.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  val num_scalars_per_ddr_word : int
  val num_scalar_64b_words : int

  val unpack_to_windows_and_negatives
    :  (module Hardcaml.Comb.S with type t = 'a)
    -> 'a
    -> 'a array * 'a array

  val create : Scope.t -> Signal.t I.t -> Signal.t O.t
  val hierarchical : ?instance:string -> Scope.t -> Signal.t Interface.Create_fn(I)(O).t
end
