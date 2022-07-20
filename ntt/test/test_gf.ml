open! Core
open Hardcaml
module Gf_z = Ntts_r_fun.Gf_z
module Gf = Ntts_r_fun.Gf_bits.Make (Bits)

let sexp_of_z z = Z.to_string z |> [%sexp_of: String.t]

let test_vector_z =
  let at_power p i =
    [ Z.((one lsl p) - of_int 10 + of_int i); Z.((one lsl p) + of_int i) ]
  in
  List.init 10 ~f:(fun i ->
      [ [ Z.of_int i ]
      ; at_power 31 i
      ; at_power 32 i
      ; at_power 63 i
      ; [ (let offset = 10 - i in
           Gf_z.(modulus - Gf_z.of_z (Z.of_int offset) |> Gf_z.to_z))
        ]
      ]
      |> List.concat
      |> Array.of_list)
  |> Array.concat
;;

let test_vector = Array.map test_vector_z ~f:Gf.of_z

let%expect_test "constants" =
  print_s [%message (Gf.zero : Gf.t) (Gf.one : Gf.t) (Gf.two : Gf.t) (Gf.modulus : Gf.t)];
  [%expect
    {|
    ((Gf.zero 0) (Gf.one 1) (Gf.two 2) (Gf.modulus 18446744069414584321)) |}]
;;

let%expect_test "test vectors" =
  print_s [%message (test_vector : Gf.t array)];
  [%expect
    {|
    (test_vector
     (0 2147483638 2147483648 4294967286 4294967296 9223372036854775798
      9223372036854775808 18446744069414584311 1 2147483639 2147483649 4294967287
      4294967297 9223372036854775799 9223372036854775809 18446744069414584312 2
      2147483640 2147483650 4294967288 4294967298 9223372036854775800
      9223372036854775810 18446744069414584313 3 2147483641 2147483651 4294967289
      4294967299 9223372036854775801 9223372036854775811 18446744069414584314 4
      2147483642 2147483652 4294967290 4294967300 9223372036854775802
      9223372036854775812 18446744069414584315 5 2147483643 2147483653 4294967291
      4294967301 9223372036854775803 9223372036854775813 18446744069414584316 6
      2147483644 2147483654 4294967292 4294967302 9223372036854775804
      9223372036854775814 18446744069414584317 7 2147483645 2147483655 4294967293
      4294967303 9223372036854775805 9223372036854775815 18446744069414584318 8
      2147483646 2147483656 4294967294 4294967304 9223372036854775806
      9223372036854775816 18446744069414584319 9 2147483647 2147483657 4294967295
      4294967305 9223372036854775807 9223372036854775817 18446744069414584320)) |}]
;;

let%expect_test "test vectors are normalized" =
  Array.iter test_vector ~f:(fun x -> assert (Bits.to_bool (Gf.is_normalized x)))
;;

let%expect_test "add" =
  Array.iter test_vector_z ~f:(fun left ->
      Array.iter test_vector_z ~f:(fun right ->
          let actual = Gf.(of_z left + of_z right |> Gf.to_z) in
          let expected = Gf_z.(of_z left + of_z right |> Gf_z.to_z) in
          if not (Z.equal actual expected)
          then
            raise_s
              [%message "add failed" (left : z) (right : z) (actual : z) (expected : z)]))
;;

let%expect_test "sub" =
  Array.iter test_vector_z ~f:(fun left ->
      Array.iter test_vector_z ~f:(fun right ->
          let actual = Gf.(of_z left - of_z right |> Gf.to_z) in
          let expected = Gf_z.(of_z left - of_z right |> Gf_z.to_z) in
          if not (Z.equal actual expected)
          then
            raise_s
              [%message "sub failed" (left : z) (right : z) (actual : z) (expected : z)]))
;;

let%expect_test "mul" =
  Array.iter test_vector_z ~f:(fun left ->
      Array.iter test_vector_z ~f:(fun right ->
          let actual = Gf.(of_z left * of_z right |> Gf.to_z) in
          let actual_normalized = Gf_z.of_z actual in
          let expected = Gf_z.(of_z left * of_z right |> Gf_z.to_z) in
          if not (Z.equal actual expected)
          then
            raise_s
              [%message
                "mul failed"
                  (left : z)
                  (right : z)
                  (actual : z)
                  (actual_normalized : Gf_z.t)
                  (expected : z)]))
;;

let%expect_test "inverse" =
  let test a =
    (* zero is not invertible *)
    if not (Z.equal a Z.zero)
    then (
      let a = Gf_z.of_z a in
      let inv_a = Gf_z.inverse a in
      let product = Gf_z.(a * inv_a) in
      (* a * (1/a) = 1 *)
      if not (Z.equal (Gf_z.to_z product) Z.one)
      then print_s [%message "failed" (a : Gf_z.t) (inv_a : Gf_z.t) (product : Gf_z.t)])
  in
  Array.iter test_vector_z ~f:test
;;

let%expect_test "omega roots" =
  let inverse, forward = Ntts_r_fun.Roots.inverse, Ntts_r_fun.Roots.forward in
  print_s [%message (inverse : Gf_z.t array) (forward : Gf_z.t array)];
  [%expect
    {|
    ((inverse
      (1 18446744069414584320 281474976710656 18446744069397807105
       17293822564807737345 70368744161280 549755813888 17870292113338400769
       13797081185216407910 1803076106186727246 11353340290879379826
       455906449640507599 17492915097719143606 1532612707718625687
       16207902636198568418 17776499369601055404 6115771955107415310
       12380578893860276750 9306717745644682924 18146160046829613826
       3511170319078647661 17654865857378133588 5416168637041100469
       16905767614792059275 9713644485405565297 5456943929260765144
       17096174751763063430 1213594585890690845 6414415596519834757
       16116352524544190054 9123114210336311365 4614640910117430873
       1753635133440165772))
     (forward
      (1 18446744069414584320 18446462594437873665 1099511627520 68719476736
       18446744069414322177 18302628881338728449 18442240469787213841
       2117504431143841456 4459017075746761332 4295002282146690441
       8548973421900915981 11164456749895610016 3968367389790187850
       4654242210262998966 1553425662128427817 7868944258580147481
       14744321562856667967 2513567076326282710 5089696809409609209
       17260140776825220475 11898519751787946856 15307271466853436433
       5456584715443070302 1219213613525454263 13843946492009319323
       16884827967813875098 10516896061424301529 4514835231089717636
       16488041148801377373 16303955383020744715 10790884855407511297
       8554224884056360729))) |}];
  let prod = Array.map2_exn inverse forward ~f:Gf_z.( * ) in
  print_s [%message (prod : Gf_z.t array)];
  [%expect
    {| (prod (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) |}];
  (* compute powers *)
  print_s
    [%message
      (Gf_z.pow inverse.(1) 2 : Gf_z.t)
        (Gf_z.pow inverse.(2) 4 : Gf_z.t)
        (Gf_z.pow inverse.(3) 8 : Gf_z.t)
        (Gf_z.pow inverse.(4) 16 : Gf_z.t)
        (Gf_z.pow inverse.(5) 32 : Gf_z.t)
        (Gf_z.pow inverse.(6) 64 : Gf_z.t)];
  [%expect
    {|
    (("Gf_z.pow (inverse.(1)) 2" 1) ("Gf_z.pow (inverse.(2)) 4" 1)
     ("Gf_z.pow (inverse.(3)) 8" 1) ("Gf_z.pow (inverse.(4)) 16" 1)
     ("Gf_z.pow (inverse.(5)) 32" 1) ("Gf_z.pow (inverse.(6)) 64" 1)) |}];
  print_s
    [%message
      (Gf_z.pow forward.(1) 2 : Gf_z.t)
        (Gf_z.pow forward.(2) 4 : Gf_z.t)
        (Gf_z.pow forward.(3) 8 : Gf_z.t)
        (Gf_z.pow forward.(4) 16 : Gf_z.t)
        (Gf_z.pow forward.(5) 32 : Gf_z.t)
        (Gf_z.pow forward.(6) 64 : Gf_z.t)];
  [%expect
    {|
    (("Gf_z.pow (forward.(1)) 2" 1) ("Gf_z.pow (forward.(2)) 4" 1)
     ("Gf_z.pow (forward.(3)) 8" 1) ("Gf_z.pow (forward.(4)) 16" 1)
     ("Gf_z.pow (forward.(5)) 32" 1) ("Gf_z.pow (forward.(6)) 64" 1)) |}]
;;
