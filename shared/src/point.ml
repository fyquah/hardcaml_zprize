module Jacobian = struct
  type 'a t =
    { x : 'a
    ; y : 'a
    ; z : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module Affine = struct
  type 'a t =
    { x : 'a
    ; y : 'a
    }
  [@@deriving sexp_of, hardcaml]
end
