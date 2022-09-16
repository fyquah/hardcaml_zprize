(** Jacobian representation of a point *)
module Jacobian : sig
  type 'a t =
    { x : 'a
    ; y : 'a
    ; z : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

(** Affine representation of a point. Can be converted to Jacobian by setting z
    = 1. *)

module Affine : sig
  type 'a t =
    { x : 'a
    ; y : 'a
    }
  [@@deriving sexp_of, hardcaml]
end
