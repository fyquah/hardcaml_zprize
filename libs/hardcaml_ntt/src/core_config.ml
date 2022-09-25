module type S = Core_config_intf.S

let ram_output_pipelining = 2

(* XXX fyquah: Figure out why changing ram_latency breaks the datapath. *)
let ram_latency = 1
let datapath_latency = ram_latency + ram_output_pipelining + Multiplier.latency
