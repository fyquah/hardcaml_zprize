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
            (** Implements the ground multiplier with luts if the hamming
             * weight of [argb] is less than
             * lut_only_hamming_weight_threshold]. Skips this optimization if
             * it's None.
            *)
        ; hybrid_hamming_weight_threshold : int option
            (** Implements the ground multiplier with a hybrid dsp+lut
             * multiplier if the hamming weight of [argb] is less than
             * lut_only_hamming_weight_threshold]. Skips this optimization
             * if it's None.
            *)
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
