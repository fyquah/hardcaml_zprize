open Config_intf

module type S = Config_intf.S

module Bls12_377 = struct
  let field_bits = 377
  let scalar_bits = 253
  let controller_log_stall_fifo_depth = 2
  let num_windows = 20

  let window_ram_partition_settings =
    (* TODO(fyquah):  Maybe move one more window to SLR0? There is ~zero logic
     * in there. 231/240 is tight, but it should be ok..?
     *)
    Some
      [ { slr = SLR0; num_windows = 10 }
      ; { slr = SLR1; num_windows = 4 }
      ; { slr = SLR2; num_windows = 6 }
      ]
  ;;
end
