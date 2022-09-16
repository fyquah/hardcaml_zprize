module type S = sig
  val addr_bits : int
  val data_bits : int
end

module type Config = sig
  module type S = S
end
