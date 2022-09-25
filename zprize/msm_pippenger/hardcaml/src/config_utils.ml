open Core

module Make (Config : Config.S) = struct
  open Config

  (* Integer divison so the first window might be slightly larger than the others. *)
  let num_windows = scalar_bits / window_size_bits
  let first_window_size_bits = scalar_bits - (window_size_bits * (num_windows - 1))

  let window_bit_sizes =
    Array.init num_windows ~f:(fun i ->
      if i = 0 then first_window_size_bits else window_size_bits)
  ;;
end
