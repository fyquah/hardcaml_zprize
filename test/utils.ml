open Core

let a_big_prime =
  Z.of_string
    "21888242871839275222246405745257275088696311157297823662689037894645226208583"
;;

let sexp_of_z z = Sexp.Atom ("0x" ^ Z.format "x" z)

let random_z =
  let random = Splittable_random.State.create Random.State.default in
  fun ~lo_incl ~hi_incl ->
    let generate =
      Bigint.gen_incl (Bigint.of_zarith_bigint lo_incl) (Bigint.of_zarith_bigint hi_incl)
    in
    Bigint.to_zarith_bigint (Quickcheck.Generator.generate ~size:1 ~random generate)
;;
