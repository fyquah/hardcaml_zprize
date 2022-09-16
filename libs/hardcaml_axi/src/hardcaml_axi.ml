(** A temporary library to expose common AXI types. This should be used until
 *  the actual Hardcaml_axi is open sourced.
 *)

module Stream = Stream
module Stream_splitter = Stream_splitter
module Lite = Lite

module type Config = Config.S

module Make (X : Config.S) = struct
  module Stream = Stream.Make (X)
  module Lite = Lite.Make (X)
end

module Axi16 = struct
  module Config = struct
    let addr_bits = 32
    let data_bits = 16
  end

  include Make (Config)
end

module Axi32 = struct
  module Config = struct
    let addr_bits = 32
    let data_bits = 32
  end

  include Make (Config)
end

module Axi64 = struct
  module Config = struct
    let addr_bits = 32
    let data_bits = 64
  end

  include Make (Config)
end

module Axi128 = struct
  module Config = struct
    let addr_bits = 32
    let data_bits = 128
  end

  include Make (Config)
end

module Axi256 = struct
  module Config = struct
    let addr_bits = 32
    let data_bits = 256
  end

  include Make (Config)
end

module Axi512 = struct
  module Config = struct
    let addr_bits = 32
    let data_bits = 512
  end

  include Make (Config)
end
