open Base

type t =
  { field_bits : int
  ; scalar_bits_by_core : int array
  ; controller_log_stall_fifo_depth : int
  ; window_size_bits : int
  ; ram_read_latency : int
  }

val all_window_size_bits : t -> int list
val input_point_bits : t -> int
val result_point_bits : t -> int
val scalar_bits : t -> int
val num_result_points : t -> int
val num_windows_for_slr : t -> int -> int
val last_window_size_bits_for_slr : t -> int -> int
val window_size_bits_for_slr : t -> int -> int
val last_window_for_core : t -> core_index:int -> int

module type S = sig
  val t : t
end

module Bls12_377 : S
