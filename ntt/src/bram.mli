open Hardcaml
open Hardcaml_xilinx

val create
  :  build_mode:Build_mode.t
  -> size:int
  -> clock:Signal.t
  -> port_a:Signal.t Ram_port.t
  -> port_b:Signal.t Ram_port.t
  -> Signal.t * Signal.t
