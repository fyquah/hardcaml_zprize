open Hardcaml

module Config = struct
  module type S = sig
    val window_size_bits : int
  end

  module Zprize = struct
    let window_size_bits = 13
  end
end

module type S = sig
  include Interface.S

  val equal : Signal.t t -> Signal.t t -> Signal.t
  val is_zero : Signal.t t -> Signal.t
end

module Make (Scalar_config : Config.S) = struct
  type 'a t =
    { scalar : 'a [@bits Scalar_config.window_size_bits]
    ; negative : 'a
    }
  [@@deriving sexp_of, hardcaml]

  let equal a b = Signal.(a.scalar ==: b.scalar)
  let is_zero a = Signal.(a.scalar ==:. 0)
end
