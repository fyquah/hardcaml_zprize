open! Base
(* Definitely want to open base here to make sure we don't use polymorphic
 * compares by accident. *)

type t =
  { coef_x : Z.t
  ; coef_y : Z.t
  ; gcd : Z.t
  }

let extended_euclidean ~x ~y =
  let gcd, coef_x, coef_y = Z.gcdext x y in
  { gcd; coef_x; coef_y }
;;
