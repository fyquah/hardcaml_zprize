open Core

let bits = 377
let bits2 = 377 * 2
let p = Ark_bls12_377_g1.modulus ()
let m = Z.((one lsl bits2) / p)
let sel_bottom x bits = Z.(x land ((one lsl bits) - one))
let drop_bottom x bits = Z.(x asr bits)
let gte a b = not (Z.lt a b)

type hex_z = Z.t

let sexp_of_hex_z x = Sexp.Atom (Printf.sprintf "0x%s" (Z.format "x" x))

let barrett_reduction ?(debug = false) ~radices a =
  let a' = sel_bottom a (bits + 2) in
  let q =
    Approx_msb_multiplier_model.approx_msb_multiply ~radices ~w:378 (drop_bottom a bits) m
    |> Fn.flip drop_bottom bits
    |> Fn.flip sel_bottom bits
  in
  let qp = sel_bottom Z.(q * p) (bits + 2) in
  let a_minus_qp =
    (* We use [sel_bottom] here to denote that we are ignoring most of the
     * upper bits.
     *)
    sel_bottom Z.(a' - qp) (bits + 2)
  in
  let by3 = Z.(a_minus_qp - (of_int 3 * p)) in
  let by2 = Z.(a_minus_qp - (of_int 2 * p)) in
  let by1 = Z.(a_minus_qp - (of_int 1 * p)) in
  let by0 = a_minus_qp in
  let a_mod_p =
    if Z.(gte by3 zero)
    then by3
    else if Z.(gte by2 zero)
    then by2
    else if Z.(gte by1 zero)
    then by1
    else if Z.(gte by0 zero)
    then by0
    else
      raise_s
        [%message
          "Unxpected state"
            (a : hex_z)
            (a' : hex_z)
            (qp : hex_z)
            (q : hex_z)
            (a_minus_qp : hex_z)]
  in
  if debug
  then
    print_s
      [%message
        "Intermediate values" (a' : hex_z) (q : hex_z) (qp : hex_z) (a_mod_p : hex_z)];
  a_mod_p
;;

let generate_z ~lo_incl ~hi_incl =
  Quickcheck.Generator.map
    ~f:Bigint.to_zarith_bigint
    (Bigint.gen_incl (Bigint.of_zarith_bigint lo_incl) (Bigint.of_zarith_bigint hi_incl))
;;

let test ?debug ~radices a =
  let obtained = barrett_reduction ?debug ~radices a in
  let expected = Z.(a mod p) in
  if not (Z.equal obtained expected)
  then
    raise_s
      [%message
        ""
          ~a:(Z.format "0x%x" a)
          ~obtained:(Z.format "0x%x" obtained)
          ~expected:(Z.format "0x%x" expected)]
;;

let test_random ~radices =
  Quickcheck.test
    (let%bind.Quickcheck.Generator a = generate_z ~lo_incl:Z.zero ~hi_incl:Z.(p - one) in
     let%bind.Quickcheck.Generator b = generate_z ~lo_incl:Z.zero ~hi_incl:Z.(p - one) in
     Quickcheck.Generator.return Z.(a * b))
    ~trials:100_000
    ~f:(test ~radices)
;;

let%expect_test "Special values" =
  test ~debug:true ~radices:[ Radix_3 ] Z.((p - one) * (p - one));
  [%expect
    {|
    ("Intermediate values"
     (a'
      0x2ae361e5c2ce90aa50d19387f20fa27378647ec3407997c0cf3622fba094800452217cc900000000000000000000000)
     (q
      0x1ae3a4617c510eac63b05c06ca1493b1a22d9f300f5138f1ef3622fba094800170b5d44300000008508bffffffffffe)
     (qp
      0xfffbd84467d81fded21378127fb0ec1d636df9331285ecee000000000000002e16ba885fffffff7af73ffffffffffe)
     (a_mod_p 0x1)) |}]
;;

let%expect_test "" =
  test
    ~debug:true
    ~radices:[ Radix_3; Radix_3; Radix_2 ]
    (Z.of_string
       "0x23f04030049c70b3af409d4e14d17998374719db49d47e2e06bc9941322d341174df4376adb5561bd14a16d0df7feabe089bb711aa0ae8be84588f7d94c61b839ca85c9342b57d9995bca4d2bac7254be5efd39871d55ca4d6fa019a30013");
  [%expect
    {|
    ("Intermediate values"
     (a'
      0x3e089bb711aa0ae8be84588f7d94c61b839ca85c9342b57d9995bca4d2bac7254be5efd39871d55ca4d6fa019a30013)
     (q
      0x156275e4da06f626ede27db2023ab34cd9eeb0d02b5600ba8e6e7f4af2038e3248e9a25a29f0c32868684817ebf818d)
     (qp
      0x6a5acce64f0912cabd20dabff35a5a7118746bfaeefe4b75c968d9244d5ee50cfee20d460e59f8c751844817ebf818d)
     (a_mod_p
      0x302e1ac4dadcc18d65269bb2bfcb095849f5ed17650bf32028a7a8da39e6213fae265c48a17dc7c61aeb1e9ae37e83)) |}]
;;

let%expect_test "Compare barret reduction vs actual mod (mixed radices)" =
  test_random ~radices:[ Radix_3; Radix_3; Radix_2 ]
;;

let%expect_test "Compare barret reduction vs actual mod (only radix 3)" =
  test_random ~radices:[ Radix_3 ]
;;

let%expect_test "Compare barrett reduction vs actual mod (only radix 2)" =
  test_random ~radices:[ Radix_2; Radix_2; Radix_2; Radix_2 ]
;;
