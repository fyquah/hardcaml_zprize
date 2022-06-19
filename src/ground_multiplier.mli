open Hardcaml

module Config : sig
  type t =
    | Verilog_multiply of { latency : int }
    | Hybrid_dsp_and_luts of { latency : int }
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
