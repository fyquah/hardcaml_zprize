open Base
open Hardcaml
open Signal

module Config : sig
  type t =
    { depth : int
    ; ground_multiplier : Ground_multiplier.Config.t
    }

  val latency : t -> int
end

val create : scope:Scope.t -> clock:t -> enable:t -> config:Config.t -> t -> t
