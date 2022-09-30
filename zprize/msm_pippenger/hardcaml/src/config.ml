open Config_intf

module type S = Config_intf.S

module Bls12_377 = struct
  let field_bits = 377
  let scalar_bits = 253
  let controller_log_stall_fifo_depth = 2
  let window_size_bits = 12

  let window_ram_partition_settings =
    Some
      [ { slr = SLR0; num_windows = 10 }
      ; { slr = SLR1; num_windows = 5 }
      ; { slr = SLR2; num_windows = 6 }
      ]
  ;;
end
