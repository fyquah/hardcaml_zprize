open Base

type twiddle_4step_config =
  { rows_per_iteration : int
  ; log_num_iterations : int
  }
[@@deriving sexp_of]

module type S = sig
  val logn : int
  val twiddle_4step_config : twiddle_4step_config option
end

module type Core_config = sig
  type nonrec twiddle_4step_config = twiddle_4step_config

  val ram_output_pipelining : int
  val ram_latency : int
  val datapath_latency : int

  module type S = S
end
