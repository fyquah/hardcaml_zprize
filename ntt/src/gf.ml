open! Base
open! Hardcaml
open! Bits

type nonrec t = t

let num_bits = 64
let zero = of_int ~width:num_bits 0
let one = of_int ~width:num_bits 1
let two = of_int ~width:num_bits 2
let epsilon = of_int ~width:num_bits ((1 lsl 32) - 1)
let modulus = of_int64 ~width:num_bits 0xFFFF_FFFF_0000_0001L
let mult_mask = of_int64 ~width:num_bits 0xFFFF_FFFF_FFFF_FFFFL

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
         Z.of_string const |> of_z ~width:num_bits)
;;

let overflow res = mux2 (msb res) Uop.(lsbs res +: epsilon) res

let add left right =
  let res = Uop.(left +: right) in
  overflow res |> overflow |> lsbs
;;

let underflow res = mux2 (msb res) Uop.(lsbs res -: epsilon) res

let sub left right =
  let res = Uop.(left +: right) in
  underflow res |> underflow |> lsbs
;;

let mul left right =
  let res = left *: right in
  let t0 = Uop.(res.:[31, 0] -: res.:[63, 48]) in
  let t0 = underflow t0 in
  let t1 = res.:[47, 32] *: epsilon in
  let final = Uop.(t0 +: t1) in
  overflow final |> lsbs
;;

let to_canonical x = mux2 Uop.(x >=: modulus) (x -: modulus) x
let negate x = mux2 (x ==:. 0) x (modulus -: to_canonical x)
let ( +: ) = add
let ( -: ) = sub
let ( *: ) = mul
