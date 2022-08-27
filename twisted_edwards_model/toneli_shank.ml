(* Obtained from https://rosettacode.org/wiki/Tonelli-Shanks_algorithm#OCaml *)

let tonelli_shank ~p n =
  let open Z in
  let two = ~$2 in
  let pp = pred p in
  let pph = pred p / two in
  let pow_mod_p a e = powm a e p in
  let legendre_p a = pow_mod_p a pph in
  if legendre_p n <> one
  then None
  else (
    let s = trailing_zeros pp in
    if s = 1
    then (
      let r = pow_mod_p n (succ p / ~$4) in
      Some (r, p - r))
    else (
      let q = pp asr s in
      let z =
        let rec find_non_square z =
          if legendre_p z = pp then z else find_non_square (succ z)
        in
        find_non_square two
      in
      let rec loop c r t m =
        if t = one
        then r, p - r
        else (
          let mp = pred m in
          let rec find_i n i =
            if n = one || i >= mp then i else find_i (n * n mod p) (succ i)
          in
          let rec exp_pow2 b e =
            if e <= zero then b else exp_pow2 (b * b mod p) (pred e)
          in
          let i = find_i t zero in
          let b = exp_pow2 c (mp - i) in
          let c = b * b mod p in
          loop c (r * b mod p) (t * c mod p) i)
      in
      Some (loop (pow_mod_p z q) (pow_mod_p n (succ q / two)) (pow_mod_p n q) ~$s)))
;;
