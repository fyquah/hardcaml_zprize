module Scalar_config = struct
  module type S = sig
    val window_size_bits : int
  end

  module Zprize = struct
    let window_size_bits = 13
  end
end

module Make (Scalar_config : Scalar_config.S) = struct
  type 'a t =
    { scalar : 'a [@bits Scalar_config.window_size_bits]
    ; negative : 'a
    }
  [@@deriving sexp_of, hardcaml]
end
