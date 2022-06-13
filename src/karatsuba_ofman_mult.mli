open Hardcaml

module Config : sig
  type t =
    | Ground_multiplier of ground_multiplier
    | Karatsubsa_ofman_stage of karatsubsa_ofman_stage

  and karatsubsa_ofman_stage =
    { post_adder_stages : int
    ; config_m0 : t
    ; config_m1 : t
    ; config_m2 : t
    }

  and ground_multiplier =
    | Verilog_multiply of { latency : int }
    | Hybrid_dsp_and_luts of { latency : int }

  val latency : t -> int

  val generate
    : ground_multiplier: ground_multiplier
    -> depth: int
    -> t
end

val hierarchical
  : enable: Signal.t
  -> config: Config.t
  -> scope: Scope.t
  -> clock:Signal.t
  -> Signal.t
  -> Signal.t
  -> Signal.t

module With_interface(M : sig val bits : int end) : sig
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

  val create : config: Config.t -> Scope.t -> Signal.t I.t -> Signal.t O.t
end
