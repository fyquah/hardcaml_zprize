open Hardcaml

module T = struct
  type 'a t =
    { is_infinity : 'a
    ; point : 'a Jacobian_point.t
    }
  [@@deriving sexp_of, hardcaml]
end

include T
module With_valid = With_valid.Wrap.Make (T)
