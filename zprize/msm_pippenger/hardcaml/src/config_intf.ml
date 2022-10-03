open Core

type slr =
  | SLR0
  | SLR1
  | SLR2
[@@deriving sexp_of]

type partition_setting =
  { num_windows : int
  ; slr : slr
  }
[@@deriving sexp_of]

module type S = sig
  val field_bits : int
  val scalar_bits : int
  val controller_log_stall_fifo_depth : int
  val num_windows : int
  val window_ram_partition_settings : partition_setting list option
end

module type Config = sig
  module type S = S

  module Bls12_377 : S
end
