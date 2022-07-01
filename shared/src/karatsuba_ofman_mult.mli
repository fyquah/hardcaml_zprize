open Hardcaml

module Config : sig
  type t =
    | Ground_multiplier of Ground_multiplier.Config.t
    | Karatsubsa_ofman_stage of karatsubsa_ofman_stage

  and karatsubsa_ofman_stage =
    { post_adder_stages : int
    ; radix : Radix.t
    ; child_config : t
    }

  val latency : t -> int
  val generate : ground_multiplier:Ground_multiplier.Config.t -> Radix.t list -> t
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
