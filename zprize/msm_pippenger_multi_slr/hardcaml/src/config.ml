open Base

type t =
  { field_bits : int
  ; scalar_bits_by_slr : int Map.M(Slr).t
  ; controller_log_stall_fifo_depth : int
  ; window_size_bits : int
  ; ram_read_latency : int
  }

let scalar_bits t = List.fold (Map.data t.scalar_bits_by_slr) ~init:0 ~f:( + )

let config_for_compute_unit t slr =
  { Pippenger_compute_unit.Pippenger_compute_unit_config.field_bits = t.field_bits
  ; scalar_bits = Map.find_exn t.scalar_bits_by_slr slr
  ; window_size_bits = t.window_size_bits
  ; ram_read_latency = t.ram_read_latency
  ; controller_log_stall_fifo_depth = t.controller_log_stall_fifo_depth
  }
;;

let num_result_points t =
  List.fold (Map.keys t.scalar_bits_by_slr) ~init:0 ~f:(fun acc slr ->
    let config = config_for_compute_unit t slr in
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

module type S = sig
  val t : t
end

module Bls12_377 : S = struct
  let t =
    { field_bits = 377
    ; scalar_bits_by_slr = Map.of_alist_exn (module Slr) [ SLR2, 253 ]
    ; controller_log_stall_fifo_depth = 2
    ; window_size_bits = 12
    ; ram_read_latency = 1
    }
  ;;
end
