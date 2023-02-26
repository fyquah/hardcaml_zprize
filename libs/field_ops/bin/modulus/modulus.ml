open Core

let () = printf "let m = Z.of_string \"%s\"" (Z.to_string (Ark_bls12_377_g1.modulus ()))
