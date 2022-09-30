type slr =
  | SLR0
  | SLR1
  | SLR2

type partition_setting =
  { num_windows : int
  ; slr : slr
  }

module type S = sig
  val field_bits : int

  (* val scalar_bits : (int * slr) list *)
  val scalar_bits : int
  val controller_log_stall_fifo_depth : int
  val window_size_bits : int
  val window_ram_partition_settings : partition_setting list option
end

module type Config = sig
  module type S = S

  module Bls12_377 : S
end
