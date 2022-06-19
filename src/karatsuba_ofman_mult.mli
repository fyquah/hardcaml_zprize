open Hardcaml

module Config : sig
  type t =
    | Ground_multiplier of Ground_multiplier.Config.t
    | Karatsubsa_ofman_stage of karatsubsa_ofman_stage

  and karatsubsa_ofman_stage =
    { post_adder_stages : int
    ; config_m0 : t
    ; config_m1 : t
    ; config_m2 : t
    }

  val latency : t -> int
  val generate : ground_multiplier:Ground_multiplier.Config.t -> depth:int -> t
end

val hierarchical
  :  enable:Signal.t
  -> config:Config.t
  -> scope:Scope.t
  -> clock:Signal.t
  -> Signal.t
  -> [ `Constant of Z.t | `Signal of Signal.t ]
  -> Signal.t

module With_interface (M : sig
  val bits : int
end) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; enable : 'a
      ; valid : 'a
      ; a : 'a
      ; b : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { c : 'a
      ; valid : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : config:Config.t -> Scope.t -> Signal.t I.t -> Signal.t O.t
end

module For_testing : sig
  val long_multiplication_with_addition
    :  (module Comb.S with type t = 'a)
    -> pivot:'a
    -> 'a
    -> 'a

  val long_multiplication_with_subtraction
    :  (module Comb.S with type t = 'a)
    -> pivot:'a
    -> 'a
    -> 'a
end
