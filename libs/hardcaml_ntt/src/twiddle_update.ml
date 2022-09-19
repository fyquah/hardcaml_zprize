open! Base

let sync_cycles =
  max
    (Twiddle_factor_stream.pipe_length + Core_config.ram_output_pipelining + 1)
    (Core_config.datapath_latency + 1)
;;

let sync_cycles_width = max 1 (Int.ceil_log2 sync_cycles)

type 'a t =
  { valid : 'a
  ; index : 'a [@bits sync_cycles_width]
  }
[@@deriving sexp_of, hardcaml]
