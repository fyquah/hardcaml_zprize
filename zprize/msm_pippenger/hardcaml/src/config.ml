module type S = Config_intf.S

module Bls12_377 = struct
  let field_bits = 377
  let scalar_bits = 253
  let controller_log_stall_fifo_depth = 2
  let window_size_bits = 12
end
