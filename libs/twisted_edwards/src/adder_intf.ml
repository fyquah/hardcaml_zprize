open Hardcaml

module type S = sig
  module Make (Num_bits : Num_bits.S) : sig
    module Xyt : module type of Xyt.Make (Num_bits)
    module Xyzt : module type of Xyzt.Make (Num_bits)

    module I : sig
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; valid_in : 'a
        ; p1 : 'a Xyzt.t
        ; p2 : 'a Xyt.t
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O : sig
      type 'a t =
        { valid_out : 'a
        ; p3 : 'a Xyzt.t
        }
      [@@deriving sexp_of, hardcaml]
    end

    val latency : Config.t -> int
    val create : config:Config.t -> Scope.t -> Signal.t I.t -> Signal.t O.t
  end
end
