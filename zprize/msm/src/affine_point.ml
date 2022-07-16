open Hardcaml
open Snarks_r_fun

module T = struct
  type 'a t = 'a Point.Affine.t =
    { x : 'a [@bits 377]
    ; y : 'a [@bits 377]
    }
  [@@deriving sexp_of, hardcaml]
end

include T
module With_valid = With_valid.Wrap.Make (T)
