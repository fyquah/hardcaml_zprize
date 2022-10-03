(** Calculates [a mod p] using barret reduction, where [p] is a compile-time known prime.

    The full barrett reduction algorithm is described in section 3 of the
    {{: https://dl.acm.org/doi/abs/10.1145/3431920.3439306} Efficient FPGA Modular Multiplication Implementation}
    paper.

    Barrett reduction comprises of the following steps

    stage 1: [q  <- approx_msb_mult(a, m)]

    stage 2: [qp <- half_width_mult(q, p)]

    stage 3: [t  <- a - qp]

    stage 4: keep subtracting [p] from [t] until [0 <= t < p]

    {!Config.t} can be used to control the exact parameters used in every
    stage.
*)

open Hardcaml

module Config : sig
  type t =
    { approx_msb_multiplier_config : Approx_msb_multiplier.Config.t
        (** Configuration of [approx_msb_mult] for stage 1. *)
    ; half_multiplier_config : Half_width_multiplier.Config.t
        (** Configuration of [half_width_mult] for stage 2. *)
    ; subtracter_stages : int
    ; num_correction_steps : int
        (** The number of correction steps required to do fine reduction. This
            number should be chosen based on the approximation error possible
            due to `approx_msb_mult` *)
    ; include_fine_reduction : bool
        (** When false, omits stage 4. This is useful to delay fine reduction
            until absolutely necessary. *)
    }

  (** Latency of the barrett reduction module for the given config. *)
  val latency : t -> int

  (** A specially tuned value for our zprize submission for bls12-377 MSM *)
  val for_bls12_377 : t
end

module With_interface (M : sig
  val bits : int
  val output_bits : int
end) : sig
  val bits : int
  val output_bits : int

  module I : sig
    type 'a t =
      { clock : 'a
      ; enable : 'a
      ; a : 'a
      ; valid : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { a_mod_p : 'a
      ; valid : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : config:Config.t -> p:Z.t -> Scope.t -> Signal.t I.t -> Signal.t O.t
  val hierarchical : config:Config.t -> p:Z.t -> Scope.t -> Signal.t I.t -> Signal.t O.t
end

(** Instantiates barrett reduction module. *)
val hierarchical
  :  scope:Scope.t
  -> config:Config.t
  -> p:Z.t
  -> clock:Signal.t
  -> enable:Signal.t
  -> Signal.t With_valid.t
  -> Signal.t With_valid.t
