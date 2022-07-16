open Hardcaml

module T = struct
  type 'a t =
    { point : 'a Affine_point.t
    ; is_infinity : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

include T
module With_valid = With_valid.Wrap.Make (T)
