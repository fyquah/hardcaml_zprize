open Hardcaml
open Hardcaml_axi

module Make (Config : Config.S) : sig
  module Mixed_add : module type of Twisted_edwards_lib.Mixed_add.Make (struct
    let num_bits = Config.field_bits
  end)

  module Top : module type of Top.Make (Config)

  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; host_to_fpga : 'a Axi512.Stream.Source.t
      ; scalar_and_input_point_ready : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { host_to_fpga_dest : 'a Axi512.Stream.Dest.t
      ; scalar : 'a
      ; scalar_valid : 'a
      ; last_scalar : 'a
      ; input_point : 'a Top.Mixed_add.Xyt.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  val hierarchical : ?instance:string -> Scope.t -> Signal.t Interface.Create_fn(I)(O).t
end
