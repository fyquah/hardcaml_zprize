module type S = sig
  include Hardcaml_ntt.Core_config.S

  val memory_layout : Memory_layout.t
  val log_num_streams : int
end

module type Top_config = sig
  module type S = S
end
