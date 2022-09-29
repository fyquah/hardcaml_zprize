open! Core

module Make (Config : Config.S) : sig
  val window_bit_sizes : int array
  val top_window_size : int
  val window_bit_offsets : int array
  val max_window_size_bits : int
  val min_window_size_bits : int
  val num_buckets : int -> int
  val scalar_to_ram_index : (module Hardcaml.Comb.S with type t = 'a) -> 'a -> 'a
end
