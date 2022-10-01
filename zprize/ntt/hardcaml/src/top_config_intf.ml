module type S = sig
  include Hardcaml_ntt.Core_config.S

  val memory_layout : Memory_layout.t
end

module type Top_config = sig
  module type S = S
end
