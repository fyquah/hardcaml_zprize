open Hardcaml

module Config : sig
  module type S = sig
    val window_size_bits : int
  end

  module Zprize : S
end

module type S = sig
  include Interface.S

  val equal : Signal.t t -> Signal.t t -> Signal.t
  val is_zero : Signal.t t -> Signal.t
end

module Make (Scalar_config : Config.S) : sig
  type 'a t =
    { scalar : 'a
    ; negative : 'a
    }
  [@@deriving sexp_of, hardcaml]

  val equal : Signal.t t -> Signal.t t -> Signal.t
  val is_zero : Signal.t t -> Signal.t
end
