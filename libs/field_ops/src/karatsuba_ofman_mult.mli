(** Multiplier using the karatsuba-ofman algorithm.

    See {{: https://en.wikipedia.org/wiki/Karatsuba_algorithm} this wikipedia
    entry} for more details for the 2-part splitting algorithm (we call this
    Radix_2 in our code). We also support a Radix_3 mode, which does a 3-part
    splitting, but utilizes similar ideas.

    The implementation here is largely based on the
    {{:https://github.com/ZcashFoundation/zcash-fpga/blob/c4c0ad918898084c73528ca231d025e36740d40c/ip_cores/util/src/rtl/karatsuba_ofman_mult.sv}
    zcash-fpga implementation of the same algorithm}
*)

open Hardcaml

module Config : sig
  module Level : sig
    type t =
      { radix : Radix.t
          (** Specified the number of splitting to be done on terms when building
          the multiplier.
      *)
      ; pre_adder_stages : int
          (** The number of pipeline stages of the pre multiplication adder. For
          a multiplication of 2 N-bit numbers, this is addition of width
          ceil(N/2)
      *)
      ; middle_adder_stages : int
          (** The number of pipeline stages of the middle adder. This is an
          adder of width round_up_to_multiple_of_2(N)
      *)
      ; post_adder_stages : int
          (** The number of pipeline stages on the final adder. For a
          multiplication of 2 N-bit numbers, this will be the number
          of pipeline stages for the final 2N-bit adder.
      *)
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

  (** Returns the latency of the adder. *)
  val latency : t -> int

  (** Generate a multiplier, where the config of all k parts are symetrical. *)
  val generate : ground_multiplier:Ground_multiplier.Config.t -> Level.t list -> t
end

(** Instantiates a hierarchical karatsuba-ofman-multiplier. When [`Constant] is
    passed for the RHS argument, the implementation will take care not to
    register the value and leave it as a constant throughout the multiplier
    tree.
*)
val hierarchical
  :  enable:Signal.t
  -> config:Config.t
  -> scope:Scope.t
  -> clock:Signal.t
  -> Signal.t
  -> [ `Constant of Z.t | `Signal of Signal.t ]
  -> Signal.t

(** Similar to [hierarchical] above, but with interfaces. *)
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
