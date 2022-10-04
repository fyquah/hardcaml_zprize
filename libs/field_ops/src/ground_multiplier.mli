(** A full "naive" multiplication module without tricks. *)

(** This module use used as the base case of more complicated recursive
    multipliers (eg: {!Karatsuba_ofman_mult} and {!Approx_msb_multiplier})

    This module offers 3 ways of implementing multipliers, which can be specified
    by {!Config.t}, they are:

    - [Verilog_multiply] - this generates a [a * b] in verilog and lets the
    synthesizer decides how to implement it. This will usually get mapped into
    DSPs.
    - [Hybrid_dsp_and_luts] - this generate a hybrid multiplier where the
    multiplier result computation is split into 2 parts, where some is done in
    DSP and some is done in LUTs.
    - [Mixed] - Generates a [Verilog_multiply] for multiplication between
    unknowns. If one of the operand is a constant, it might optionally implement
    a custom lut-based multiplier depending on its value. Read on for more info.

    [Mixed] and [Hybrid_dsp_and_luts] also support a
    [lut_only_hamming_weight_threshold] that uses LUTs-only to implement
    multiplication when [b] is a constant and its
    {{:https://en.wikipedia.org/wiki/Non-adjacent_form} Non-Adjacent Form}
    has a low hamming weight. This is useful to tune LUT vs DSP trade-off in
    designs. The multiplier implemented in LUTs uses the high-school
    multiplication algorithm.
*)

open Hardcaml

module Config : sig
  type t =
    | Verilog_multiply of { latency : int }
    | Hybrid_dsp_and_luts of
        { latency : int
        ; lut_only_hamming_weight_threshold : int
        }
    | Mixed of
        { latency : int
        ; lut_only_hamming_weight_threshold : int option
        ; hybrid_hamming_weight_threshold : int option
        }
    | Specialized_43_bit_multiply
  [@@deriving sexp_of]

  val latency : t -> int
end

val create
  :  clock:Signal.t
  -> enable:Signal.t
  -> config:Config.t
  -> Signal.t
  -> Signal.t
  -> Signal.t

module For_testing : sig
  val long_multiplication_with_addition
    :  (module Comb.S with type t = 'a)
    -> pivot:'a
    -> 'a
    -> 'a

  val specialized_43_bit_multiply : (module Comb.S with type t = 'a) -> 'a -> 'a -> 'a
end
