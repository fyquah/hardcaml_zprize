open Hardcaml

module Config : sig
  module Level : sig
    type t =
      { radix : Radix.t
      ; pre_adder_stages : int
      ; post_adder_stages : int
      }
    [@@deriving sexp_of]
  end

  type t =
    | Ground_multiplier of Ground_multiplier.Config.t
    | Karatsubsa_ofman_stage of karatsubsa_ofman_stage

  and karatsubsa_ofman_stage =
    { level : Level.t
    ; child_config : t
    }

  val latency : t -> int
  val generate : ground_multiplier:Ground_multiplier.Config.t -> Level.t list -> t
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
  val hierarchical : config:Config.t -> Scope.t -> Signal.t I.t -> Signal.t O.t
end
