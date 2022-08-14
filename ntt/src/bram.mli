open Hardcaml
open Hardcaml_xilinx

val create
  :  build_mode:Build_mode.t
  -> size:int
  -> clock:Signal.t
  -> port_a:Signal.t Ram_port.t
  -> port_b:Signal.t Ram_port.t
  -> Signal.t * Signal.t

type write_port =
  { address : Signal.t
  ; data : Signal.t
  ; enable : Signal.t
  }

type read_port =
  { address : Signal.t
  ; enable : Signal.t
  }

val create_dual
  :  build_mode:Build_mode.t
  -> size:int
  -> clock:Signal.t
  -> clear:Signal.t
  -> flip:Signal.t
  -> write_port_a:write_port
  -> write_port_b:write_port
  -> read_port_a:read_port
  -> read_port_b:read_port
  -> Signal.t * Signal.t
