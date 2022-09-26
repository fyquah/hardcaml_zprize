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

  let window_bit_offsets =
    let rec compute_offsets i data cur_offset =
      if i = num_windows
      then data
      else compute_offsets (i + 1) (cur_offset :: data) (cur_offset + window_bit_sizes.(i))
    in
    compute_offsets 0 [] 0 |> Array.of_list
  ;;

  let max_window_size_bits =
    Option.value_exn (Array.max_elt ~compare:Int.compare window_bit_sizes)
  ;;
end
