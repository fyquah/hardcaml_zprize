open Base

module Pippenger_compute_unit_config =
  Pippenger_compute_unit.Pippenger_compute_unit_config

module For_core = struct
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

let scalar_bits t = Array.fold t.for_cores ~init:0 ~f:(fun acc c -> acc + c.scalar_bits)
let input_point_bits t = 3 * t.field_bits
let result_point_bits t = 4 * t.field_bits

let config_for_compute_unit t ~core_index =
  { Pippenger_compute_unit.Pippenger_compute_unit_config.field_bits = t.field_bits
  ; scalar_bits = t.for_cores.(core_index).scalar_bits
  ; window_size_bits = t.window_size_bits
  ; ram_read_latency = t.ram_read_latency
  ; controller_log_stall_fifo_depth = t.controller_log_stall_fifo_depth
  }
;;

let all_window_size_bits t =
  let rec loop ~core_index =
    if core_index = Array.length t.for_cores
    then []
    else (
      let config_compute_units = config_for_compute_unit t ~core_index in
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
  Array.foldi t.for_cores ~init:0 ~f:(fun i acc _ ->
    let config = config_for_compute_unit t ~core_index:i in
    acc + Pippenger_compute_unit.Pippenger_compute_unit_config.num_result_points config)
;;

let num_windows_for_core t ~core_index =
  Pippenger_compute_unit.Pippenger_compute_unit_config.num_windows
    (config_for_compute_unit t ~core_index)
;;

let last_window_size_bits_for_core t ~core_index =
  Pippenger_compute_unit.Pippenger_compute_unit_config.last_window_size_bits
    (config_for_compute_unit t ~core_index)
;;

let window_size_bits_for_core t ~core_index =
  (config_for_compute_unit t ~core_index).window_size_bits
;;

let last_window_for_core t ~core_index =
  let num_windows_before_this_core = ref 0 in
  for i = 0 to core_index - 1 do
    num_windows_before_this_core
      := !num_windows_before_this_core + num_windows_for_core t ~core_index:i
  done;
  !num_windows_before_this_core + num_windows_for_core t ~core_index - 1
;;

module type S = sig
  val t : t
end

module Bls12_377 : S = struct
  let t =
    { field_bits = 377
    ; for_cores =
        [| { scalar_bits = 84; slr = SLR0 }
         ; { scalar_bits = 84; slr = SLR1 }
         ; { scalar_bits = 85; slr = SLR2 }
        |]
    ; controller_log_stall_fifo_depth = 2
    ; window_size_bits = 12
    ; ram_read_latency = 1
    }
  ;;
end
