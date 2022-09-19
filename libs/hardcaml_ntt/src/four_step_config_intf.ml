module type S = sig
  include Core_config.S

  val logcores : int
end

module type Four_step_config = sig
  module type S = S
end
