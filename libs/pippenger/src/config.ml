module type S = Config_intf.S

module Zprize = struct
  let num_windows = 7
  let affine_point_bits = 377 * 2
  let pipeline_depth = 150
  let log_stall_fifo_depth = 2
end
