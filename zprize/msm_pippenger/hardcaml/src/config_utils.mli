module Make (Config : Config.S) : sig
  val num_windows : int
  val window_bit_sizes : int array
  val window_bit_offsets : int array
  val max_window_size_bits : int
end
