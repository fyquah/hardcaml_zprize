open Base

module Pippenger_compute_unit_config =
  Pippenger_compute_unit.Pippenger_compute_unit_config

type t =
  { field_bits : int
  ; scalar_bits_by_core : int array
  ; controller_log_stall_fifo_depth : int
  ; window_size_bits : int
  ; ram_read_latency : int
  }

let scalar_bits t = Array.fold t.scalar_bits_by_core ~init:0 ~f:( + )
let input_point_bits t = 3 * t.field_bits
let result_point_bits t = 4 * t.field_bits

let config_for_compute_unit t index =
  { Pippenger_compute_unit.Pippenger_compute_unit_config.field_bits = t.field_bits
  ; scalar_bits = t.scalar_bits_by_core.(index)
  ; window_size_bits = t.window_size_bits
  ; ram_read_latency = t.ram_read_latency
  ; controller_log_stall_fifo_depth = t.controller_log_stall_fifo_depth
  }
;;

let all_window_size_bits t =
  let rec loop ~core_index =
    if core_index = Array.length t.scalar_bits_by_core
    then []
    else (
      let config_compute_units = config_for_compute_unit t core_index in
      let hd =
        let num_windows =
          Pippenger_compute_unit_config.num_windows config_compute_units
        in
        List.init num_windows ~f:(fun i ->
          if i = num_windows - 1
          then Pippenger_compute_unit_config.last_window_size_bits config_compute_units
          else config_compute_units.window_size_bits)
      in
      hd @ loop ~core_index:(core_index + 1))
  in
  loop ~core_index:0
;;

let num_result_points t =
  Array.foldi t.scalar_bits_by_core ~init:0 ~f:(fun i acc _ ->
    let config = config_for_compute_unit t i in
    acc + Pippenger_compute_unit.Pippenger_compute_unit_config.num_result_points config)
;;

let num_windows_for_slr t slr =
  Pippenger_compute_unit.Pippenger_compute_unit_config.num_windows
    (config_for_compute_unit t slr)
;;

let last_window_size_bits_for_slr t slr =
  Pippenger_compute_unit.Pippenger_compute_unit_config.last_window_size_bits
    (config_for_compute_unit t slr)
;;

let window_size_bits_for_slr t slr = (config_for_compute_unit t slr).window_size_bits

let last_window_for_core t ~core_index =
  let num_windows_before_this_core = ref 0 in
  for i = 0 to core_index - 1 do
    num_windows_before_this_core
      := !num_windows_before_this_core + num_windows_for_slr t i
  done;
  !num_windows_before_this_core + num_windows_for_slr t core_index - 1
;;

module type S = sig
  val t : t
end

module Bls12_377 : S = struct
  let t =
    { field_bits = 377
    ; scalar_bits_by_core = [| 253 |]
    ; controller_log_stall_fifo_depth = 2
    ; window_size_bits = 12
    ; ram_read_latency = 1
    }
  ;;
end
