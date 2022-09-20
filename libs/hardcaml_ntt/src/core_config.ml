module type S = Core_config_intf.S

let ram_output_pipelining = 1
let ram_latency = 1
let datapath_latency = ram_latency + ram_output_pipelining + Multiplier.latency
