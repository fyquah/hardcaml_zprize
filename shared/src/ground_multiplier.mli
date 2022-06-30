open Hardcaml

module Config : sig
  type t =
    | Verilog_multiply of { latency : int }
    | Hybrid_dsp_and_luts of { latency : int }
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
  val specialized_43_bit_multiply : (module Comb.S with type t = 'a) -> 'a -> 'a -> 'a
end
