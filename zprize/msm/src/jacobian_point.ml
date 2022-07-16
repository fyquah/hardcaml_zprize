open Hardcaml
open Snarks_r_fun

module T = struct
  type 'a t = 'a Point.Jacobian.t =
    { x : 'a [@bits 377]
    ; y : 'a [@bits 377]
    ; z : 'a [@bits 377]
    }
  [@@deriving sexp_of, hardcaml]

  let p = Ark_bls12_377_g1.modulus ()
  let r = Z.((one lsl log2up p) mod p)

  let of_affine_point ({ x; y } : _ Affine_point.t) =
    { x; y; z = Signal.of_z ~width:377 r }
  ;;
end

include T
module With_valid = With_valid.Wrap.Make (T)
