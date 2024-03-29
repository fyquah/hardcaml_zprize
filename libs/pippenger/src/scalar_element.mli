module Config : sig
  module type S = sig
    val window_size_bits : int
  end

  module Zprize : S
end

module Make (Scalar_config : Config.S) : sig
  type 'a t =
    { scalar : 'a
    ; negative : 'a
    }
  [@@deriving sexp_of, hardcaml]
end
