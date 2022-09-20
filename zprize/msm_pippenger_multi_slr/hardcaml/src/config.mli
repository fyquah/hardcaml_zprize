open Base

module For_core : sig
  type t =
    { scalar_bits : int
    ; slr : Slr.t
    }
end

type t =
  { field_bits : int
  ; for_cores : For_core.t array
  ; controller_log_stall_fifo_depth : int
  ; window_size_bits : int
  ; ram_read_latency : int
  }

val all_window_size_bits : t -> int list
val input_point_bits : t -> int
val result_point_bits : t -> int
val scalar_bits : t -> int
val num_result_points : t -> int
val num_windows_for_core : t -> core_index:int -> int
val last_window_size_bits_for_core : t -> core_index:int -> int
val window_size_bits_for_core : t -> core_index:int -> int
val last_window_for_core : t -> core_index:int -> int

module type S = sig
  val t : t
end

module Bls12_377 : S
