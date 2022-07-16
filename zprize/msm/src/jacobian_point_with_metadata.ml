open Hardcaml

module T = struct
  type 'a t =
    { requires_extra_doubling : 'a
    ; point : 'a Jacobian_point_or_infinity.t
    }
  [@@deriving sexp_of, hardcaml]
end

include T
module With_valid = With_valid.Wrap.Make (T)
