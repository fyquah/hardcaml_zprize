open! Core
open Hardcaml

module Scalar = Pippenger.Scalar_element.Make (struct
  let window_size_bits = 13
end)

module Controller =
  Pippenger.Controller_half_rate.Make
    (struct
      let num_windows = 10
      let affine_point_bits = 377 * 3
      let datapath_depth = 200
      let pipeline_depth = (datapath_depth + 1) / 2
      let log_stall_fifo_depth = 2
    end)
    (Scalar)

module Circuit = Circuit.With_interface (Controller.I) (Controller.O)

let () =
  let scope = Scope.create ~flatten_design:false () in
  let circ =
    Circuit.create_exn
      ~name:"pippenger_controller"
      (Controller.For_synthesis.create scope)
  in
  Rtl.print ~database:(Scope.circuit_database scope) Verilog circ
;;
