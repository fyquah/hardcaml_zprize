open Hardcaml
open Signal

module Config : sig
  type fn = Field_ops_lib.Ec_fpn_ops_config.fn =
    { latency : int
    ; impl : scope:Scope.t -> clock:t -> enable:t -> t -> t option -> t
    }

  type t =
    { multiply : fn
    ; reduce : fn
    ; adder_stages : int
    ; subtractor_stages : int
    ; doubler_stages : int
    ; p : Z.t
    ; a : Z.t
    ; d : Z.t
    }

  module For_bls12_377 : sig
    val with_barrett_reduction : t Lazy.t
  end
end

module Make (Num_bits : Num_bits.S) : sig
  module Xyt : module type of Xyt.Make (Num_bits)
  module Xyzt : module type of Xyzt.Make (Num_bits)

  module I : sig
    type 'a t =
      { clock : 'a
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
