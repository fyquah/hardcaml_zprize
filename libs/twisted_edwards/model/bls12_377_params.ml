open Core
module C = Conversions

let weierstrass =
  lazy (Weierstrass_curve.create_params ~a:Z.zero ~b:Z.one ~alpha:Z.minus_one)
;;

let twisted_edwards =
  Lazy.map ~f:C.weierstrass_params_to_twisted_edwards_params weierstrass
;;
