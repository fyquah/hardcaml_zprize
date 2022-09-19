(** Wraps the MSM pippenger top module with 512 bit AXI stream interfaces that
    can be driven by the AWS shell when compiled with Vitis.

    The 512 bit stream is first converted into a 256 bit stream so that it can
    efficently be crossed into all SLRs, and then is unpacked and inputted into
    the controller. Input to the controller is double buffered so that we
    maximise performance.
*)

open Hardcaml
open Hardcaml_axi

module Make (Config : Config.S) : sig
  module Top : module type of Top.Make (Config)

  module I : sig
    type 'a t =
      { ap_clk : 'a
      ; ap_rst_n : 'a
      ; host_to_fpga : 'a Axi512.Stream.Source.t
      ; fpga_to_host_dest : 'a Axi512.Stream.Dest.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { fpga_to_host : 'a Axi512.Stream.Source.t
      ; host_to_fpga_dest : 'a Axi512.Stream.Dest.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  val hierarchical
    :  build_mode:Build_mode.t
    -> Scope.t
    -> Signal.t Interface.Create_fn(I)(O).t
end
