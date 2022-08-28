module type S = sig
  val field_bits : int
  val scalar_bits : int
  val controller_log_stall_fifo_depth : int
  val window_size_bits : int
end

module type Config = sig
  module type S = S

  module Bls12_377 : S
end
