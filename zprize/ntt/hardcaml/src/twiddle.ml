open! Base
module Gf = Gf_bits.Make (Hardcaml.Signal)

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { w : 'a [@bits Gf.num_bits] } [@@deriving sexp_of, hardcaml]
end

let create ~logn:_ ~size:_ _scope (_i : _ I.t) = O.Of_signal.of_int 0
