type slr =
  | SLR0
  | SLR1
  | SLR2

module type S = sig
  val field_bits : int

  (* val scalar_bits : (int * slr) list *)
  val scalar_bits : int
  val controller_log_stall_fifo_depth : int
  val window_size_bits : int
  val ram_read_latency : int
end

module type Config = sig
  module type S = S

  module Bls12_377 : S
end
