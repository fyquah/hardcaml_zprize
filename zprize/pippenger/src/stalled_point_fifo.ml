open! Base
open! Hardcaml
open Signal

module Make (Config : Config.S) = struct
  open Config

  let log_num_windows = Int.ceil_log2 num_windows
  let stall_fifo_depth = 1 lsl log_stall_fifo_depth

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; push : 'a
      ; scalar : 'a [@bits window_size_bits]
      ; window : 'a [@bits log_num_windows]
      ; affine_point : 'a [@bits affine_point_bits]
      ; pop : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { not_empty : 'a
      ; full : 'a
      ; affine_point_out : 'a [@bits affine_point_bits]
      ; scalar_out : 'a [@bits window_size_bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create_window _scope (i : _ I.t) ~window =
    let read_address = zero log_stall_fifo_depth in
    let write_address = zero log_stall_fifo_depth in
    let write_enable = gnd in
    let write_data = zero window_size_bits in
    let scalar =
      memory
        stall_fifo_depth
        ~write_port:{ write_clock = i.clock; write_address; write_enable; write_data }
        ~read_address
    in
    ()
  ;;

  let create _scope (_i : _ I.t) = ()
end
