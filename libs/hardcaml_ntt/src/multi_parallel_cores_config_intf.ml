module type S = sig
  include Four_step_config.S

  val logblocks : int
end

module type Multi_parallel_cores_config = sig
  module type S = S
end
