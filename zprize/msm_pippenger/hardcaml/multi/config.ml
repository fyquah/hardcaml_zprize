open Base

type t =
  { field_bits : int
  ; scalar_bits_by_slr : int Map.M(Slr).t
  ; controller_log_stall_fifo_depth : int
  ; window_size_bits : int
  ; ram_read_latency : int
  }

let scalar_bits t = List.fold (Map.data t.scalar_bits_by_slr) ~init:0 ~f:( + )

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
