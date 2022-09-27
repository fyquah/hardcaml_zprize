open Base

module type S = sig
  (** Log (base 2) of core transform size.

      For the 4 step algorithm, this is half the total transform size.
  *)
  val logn : int

  (** Enable support for the twiddle pass used in the 4 step algorithm. *)
  val support_4step_twiddle : bool

  (** {2 Configuration of multiple core instantiations} *)

  (** Log (base 2) number of cores to instantiate per block. Each block has a
     single controller component. *)
  val logcores : int

  (** Log (base 2) of number of blocks of cores to instantiate. *)
  val logblocks : int
end

module type Core_config = sig
  (** {2 Constant values used for datapath pipelining} *)

  val ram_output_pipelining : int
  val ram_latency : int
  val datapath_latency : int

  (** Core configuration signature. *)
  module type S = S
end
