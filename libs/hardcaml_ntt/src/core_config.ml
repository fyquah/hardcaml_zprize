module type S = Core_config_intf.S

(* Change [ram_output_pipelining] and [ram_latency] to experiment with various
 * pipelinling options.
 *)

(** [ram_output_pipelining] dictates of additional pipelining in the datapath.
 *  from the ram output. This pipelining is _in addition_ to [ram_latency]
 *  above, and only affects the datapath.
 *)
let ram_output_pipelining = 1

(** [ram_latency] dictates the latency of the ram when instantiating the XPM
 * memory.
 *)
let ram_latency = 1

let datapath_latency = ram_latency + ram_output_pipelining + Multiplier.latency
