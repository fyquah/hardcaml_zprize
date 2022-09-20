open Hardcaml
module Axi256 = Hardcaml_axi.Axi256

module Make (Config : Config.S) : sig
  module Top : module type of Top.Make (Config)

  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; host_to_fpga : 'a Axi256.Stream.Source.t
      ; fpga_to_host_dest : 'a Axi256.Stream.Dest.t
      ; scalar_and_input_point_ready : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { scalar : 'a
      ; scalar_valid : 'a
      ; last_scalar : 'a
      ; input_point : 'a Top.Mixed_add.Xyt.t
      ; result_point_ready : 'a
      ; host_to_fpga_dest : 'a Axi256.Stream.Dest.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  val hierarchical : ?instance:string -> Scope.t -> Signal.t Interface.Create_fn(I)(O).t
end
