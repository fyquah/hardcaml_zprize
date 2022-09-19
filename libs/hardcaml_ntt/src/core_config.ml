type twiddle_4step_config = Core_config_intf.twiddle_4step_config [@@deriving sexp_of]

module type S = Core_config_intf.S

let ram_output_pipelining = 1
let ram_latency = 1
let datapath_latency = ram_latency + ram_output_pipelining + Multiplier.latency
