open Base

let a_big_prime =
  Z.of_string "21888242871839275222246405745257275088696311157297823662689037894645226208583"
;;

let sexp_of_z z = Sexp.Atom ("0x" ^ (Z.format "x" z))
