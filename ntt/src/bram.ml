open! Base
open Hardcaml
open Signal

let create ~build_mode ~size ~clock ~port_a ~port_b =
  Hardcaml_xilinx.True_dual_port_ram.create
    ~build_mode
    ()
    ~clock_a:clock
    ~clock_b:clock
    ~clear_a:gnd
    ~clear_b:gnd
    ~size
    ~port_a
    ~port_b
;;
