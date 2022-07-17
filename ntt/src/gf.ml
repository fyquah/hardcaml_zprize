open! Base
open! Hardcaml
open! Bits

module Make (Bits : Comb.S) = struct
  open Bits

  type nonrec t = t

  let compare a b = if equal (a <: b) vdd then -1 else if equal (a >: b) vdd then 1 else 0
  let equal a b = equal (a ==: b) vdd

  let sexp_of_t t =
    Bits.to_z ~signedness:Unsigned t |> Z.to_string |> [%sexp_of: String.t]
  ;;

  let num_bits = 64
  let zero = of_int ~width:num_bits 0
  let one = of_int ~width:num_bits 1
  let two = of_int ~width:num_bits 2
  let epsilon = of_int ~width:num_bits ((1 lsl 32) - 1)
  let modulus_z = Z.of_string "0xFFFF_FFFF_0000_0001"
  let modulus = of_z ~width:num_bits modulus_z
  let mult_mask = of_int64 ~width:num_bits 0xFFFF_FFFF_FFFF_FFFFL
  let is_normalized x = Uop.(x <: modulus)
  let to_canonical x = mux2 (is_normalized x) x (x -: modulus)
  let of_z z = Z.erem z modulus_z |> Bits.of_z ~width:64
  let to_z = Bits.to_z ~signedness:Unsigned

  let omega =
    [| "1"
     ; (* for a domain of 2^0 *)
       "18446744069414584320"
     ; (* for a domain of 2^1 *)
       "281474976710656"
     ; (* for a domain of 2^2 *)
       "18446744069397807105"
     ; (* for a domain of 2^3 *)
       "17293822564807737345"
     ; (* for a domain of 2^4 *)
       "70368744161280"
     ; (* for a domain of 2^5 *)
       "549755813888"
     ; (* for a domain of 2^6 *)
       "17870292113338400769"
     ; (* for a domain of 2^7 *)
       "13797081185216407910"
     ; (* for a domain of 2^8 *)
       "1803076106186727246"
     ; (* for a domain of 2^9 *)
       "11353340290879379826"
     ; (* for a domain of 2^10 *)
       "455906449640507599"
     ; (* for a domain of 2^11 *)
       "17492915097719143606"
     ; (* for a domain of 2^12 *)
       "1532612707718625687"
     ; (* for a domain of 2^13 *)
       "16207902636198568418"
     ; (* for a domain of 2^14 *)
       "17776499369601055404"
     ; (* for a domain of 2^15 *)
       "6115771955107415310"
     ; (* for a domain of 2^16 *)
       "12380578893860276750"
     ; (* for a domain of 2^17 *)
       "9306717745644682924"
     ; (* for a domain of 2^18 *)
       "18146160046829613826"
     ; (* for a domain of 2^19 *)
       "3511170319078647661"
     ; (* for a domain of 2^20 *)
       "17654865857378133588"
     ; (* for a domain of 2^21 *)
       "5416168637041100469"
     ; (* for a domain of 2^22 *)
       "16905767614792059275"
     ; (* for a domain of 2^23 *)
       "9713644485405565297"
     ; (* for a domain of 2^24 *)
       "5456943929260765144"
     ; (* for a domain of 2^25 *)
       "17096174751763063430"
     ; (* for a domain of 2^26 *)
       "1213594585890690845"
     ; (* for a domain of 2^27 *)
       "6414415596519834757"
     ; (* for a domain of 2^28 *)
       "16116352524544190054"
     ; (* for a domain of 2^29 *)
       "9123114210336311365"
     ; (* for a domain of 2^30 *)
       "4614640910117430873"
     ; (* for a domain of 2^31 *)
       "1753635133440165772" (* for a domain of 2^32 *)
    |]
    |> Array.map ~f:(fun const ->
           (* XXX aray: Assert the constants really fit in 64 bits. *)
           Z.of_string const |> of_z)
  ;;

  let overflow res = mux2 (msb res) Uop.(lsbs res +: epsilon) res

  let _add left right =
    let res = Uop.(left +: right) in
    overflow res |> overflow |> lsbs
  ;;

  let add left right =
    let res = Uop.(left +: right) in
    mux2 (res >=: ue modulus) (res -: ue modulus) res |> lsbs
  ;;

  let underflow res = mux2 (msb res) Uop.(lsbs res -: epsilon) res

  let _sub left right =
    let res = Uop.(left -: right) in
    underflow res |> underflow |> lsbs
  ;;

  let sub left right =
    let res = Uop.(left -: right) in
    mux2 (msb res) (res +: ue modulus) res |> lsbs
  ;;

  let mul ?(pipe = Fn.id) left right =
    let res = left *: right |> pipe in
    let t0 = Uop.(res.:[63, 0] -: res.:[127, 96]) in
    let t0 = underflow t0 |> pipe in
    let t1 =
      let r = res.:[95, 64] in
      Uop.((r @: Bits.zero 32) -: r) |> pipe
    in
    let final = t0 +: t1 in
    mux2 (final >=: ue modulus) (final -: ue modulus) final |> lsbs |> pipe
  ;;

  let to_bits t = t
  let of_bits t = t
  let negate x = mux2 (x ==:. 0) x (modulus -: to_canonical x)
  let ( +: ) = add
  let ( -: ) = sub
  let ( *: ) a b = mul a b
end

module Z = struct
  type t = Z.t

  let sexp_of_t t = Z.to_string t |> [%sexp_of: String.t]

  (* This is a solinas prime (2^64 - 2^32 + 1) which has a fast modular
     reduction algorithm. *)
  let modulus = Z.((one lsl 64) - (one lsl 32) + one)
  let of_z t = Z.(erem t modulus)
  let add a b = Z.((a + b) mod modulus)
  let sub a b = Z.(erem (a - b) modulus)
  let mul a b = Z.(a * b mod modulus)
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let of_int = Z.of_int
  let to_z t = t

  (* Inverse computed by the extended euclidian algorithm *)
  let inverse a =
    let rec f t nt r nr =
      if Z.equal nr Z.zero
      then t, r
      else (
        let q = Z.div r nr in
        let t = nt
        and nt = Z.(t - (q * nt)) in
        let r = nr
        and nr = Z.(r - (q * nr)) in
        f t nt r nr)
    in
    let t, r = f Z.zero Z.one modulus a in
    if Z.compare r Z.one > 0 then raise_s [%message "Not invertable" (a : t)];
    if Z.compare t Z.zero < 0 then Z.(t + modulus) else t
  ;;

  let rec pow a n =
    if n <= 0
    then raise_s [%message "pow must be raised to positive power" (n : int)]
    else if n = 1
    then a
    else a * pow a Int.(n - 1)
  ;;

  let pp fmt z = Caml.Format.fprintf fmt "%s" (sexp_of_t z |> Sexplib.Sexp.to_string)
end
