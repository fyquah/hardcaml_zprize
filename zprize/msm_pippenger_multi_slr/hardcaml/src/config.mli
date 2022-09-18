open Base

type t =
  { field_bits : int
  ; scalar_bits_by_slr : int Map.M(Slr).t
  ; controller_log_stall_fifo_depth : int
  ; window_size_bits : int
  ; ram_read_latency : int
  }

val scalar_bits : t -> int
val num_result_points : t -> int
val num_windows_for_slr : t -> Slr.t -> int
val last_window_size_bits_for_slr : t -> Slr.t -> int
val window_size_bits_for_slr : t -> Slr.t -> int

module type S = sig
  val t : t
end

module Bls12_377 : S
