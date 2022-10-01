open Core

module Make (Config : Config.S) = struct
  open Config

  (* Integer divison so the first window might be slightly larger than the others. *)
  let lower_window_size = scalar_bits / num_windows
  let num_higher_windows = scalar_bits - (lower_window_size * num_windows)

  let window_bit_sizes =
    Array.init num_windows ~f:(fun i ->
      if i < num_higher_windows then lower_window_size + 1 else lower_window_size)
  ;;

  let top_window_size = window_bit_sizes.(num_windows - 1)

  let window_bit_offsets =
    let rec compute_offsets i data cur_offset =
      if i = num_windows
      then data
      else compute_offsets (i + 1) (cur_offset :: data) (cur_offset + window_bit_sizes.(i))
    in
    compute_offsets 0 [] 0 |> List.rev |> Array.of_list
  ;;

  let max_window_size_bits =
    Option.value_exn (Array.max_elt ~compare:Int.compare window_bit_sizes)
  ;;

  let min_window_size_bits =
    Option.value_exn (Array.max_elt ~compare:Int.compare window_bit_sizes)
  ;;

  let num_buckets window =
    (* -1 trick -> doesn't apply to last window *)
    if window >= num_windows
    then 0
    else if window = num_windows - 1
    then (1 lsl window_bit_sizes.(window)) - 1
    else 1 lsl (window_bit_sizes.(window) - 1)
  ;;

  let scalar_to_ram_index
    (type a)
    (module Comb : Hardcaml.Comb.S with type t = a)
    (scalar : a)
    =
    Comb.(scalar -:. 1)
  ;;
end
