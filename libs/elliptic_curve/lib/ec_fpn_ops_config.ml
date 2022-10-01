open Hardcaml
open Signal

type fn =
  { latency : int
  ; impl : scope:Scope.t -> clock:t -> enable:t -> t -> t option -> t
  }

type t =
  { multiply : fn
  ; square : fn
  ; reduce : fn
  ; coarse_reduce : fn
  ; p : Z.t
  }

let reduce config ~scope ~clock ~enable x =
  config.reduce.impl ~scope ~clock ~enable x None
;;

let coarse_reduce config ~scope ~clock ~enable x =
  config.coarse_reduce.impl ~scope ~clock ~enable x None
;;

let multiply_latency ~reduce (t : t) =
  t.multiply.latency + if reduce then t.reduce.latency else 0
;;

let square_latency ~reduce (t : t) =
  t.square.latency + if reduce then t.reduce.latency else 0
;;
