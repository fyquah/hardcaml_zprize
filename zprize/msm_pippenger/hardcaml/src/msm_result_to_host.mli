(** Takes in points from the MSM block and packs them input a 512bit stream
    where every coordinate and point has been aligned to 64 bits, and can then
    be sent up to a central controller and back to the host.

    Optionally drops or sends the t coordinate. *)

open Hardcaml
open Hardcaml_axi

module Make (Config : Config.S) : sig
  module Mixed_add : module type of Twisted_edwards_lib.Mixed_add.Make (struct
    let num_bits = Config.field_bits
  end)

  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; result_point : 'a Mixed_add.Xyzt.t
      ; result_point_valid : 'a
      ; last_result_point : 'a
      ; fpga_to_host_dest : 'a Axi512.Stream.Dest.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { fpga_to_host : 'a Axi512.Stream.Source.t
      ; result_point_ready : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val hierarchical
    :  ?instance:string
    -> drop_t:Signal.t
    -> Scope.t
    -> Signal.t Interface.Create_fn(I)(O).t
end
