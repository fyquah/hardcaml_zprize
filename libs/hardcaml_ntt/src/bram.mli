(** Double buffered (Ultra)ram blocks which provide 2 read and 2 write ports.

   Internally we instantiate 2 seperate rams to provide the require port
   density. The write ports access one ram, while the read ports access the
   other. The [flip] signal swaps which rams are read and written. *)

open Hardcaml
open Hardcaml_xilinx

val create
  :  Scope.t
  -> build_mode:Build_mode.t
  -> size:int
  -> read_latency:int
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
  :  Scope.t
  -> build_mode:Build_mode.t
       (** Choose between implementation for simulation or synthesis *)
  -> size:int (** Depth of RAM *)
  -> read_latency:int
  -> clock:Signal.t
  -> clear:Signal.t
  -> flip:Signal.t (** Swap which ram is accessed for reading and writing. *)
  -> write_port_a:write_port
  -> write_port_b:write_port
  -> read_port_a:read_port
  -> read_port_b:read_port
  -> Signal.t * Signal.t
