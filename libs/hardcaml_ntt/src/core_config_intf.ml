open Base

module type S = sig
  val logn : int
  val support_4step_twiddle : bool

  (** {2 Configuration of multiple core instantiations} *)

  val logcores : int
  val logblocks : int
end

module type Core_config = sig
  val ram_output_pipelining : int
  val ram_latency : int
  val datapath_latency : int

  module type S = S
end
