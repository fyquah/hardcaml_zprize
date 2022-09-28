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

let barrett_reduction ?(debug = false) ~levels ~num_correction_steps a =
  let a' = sel_bottom a (bits + num_correction_steps) in
  let q =
    Approx_msb_multiplier_model.approx_msb_multiply ~levels ~w:378 (drop_bottom a bits) m
    |> Fn.flip drop_bottom bits
    |> Fn.flip sel_bottom bits
  in
  let qp = sel_bottom Z.(q * p) (bits + num_correction_steps) in
  let a_minus_qp =
    (* We use [sel_bottom] here to denote that we are ignoring most of the
     * upper bits.
     *)
    sel_bottom Z.(a' - qp) (bits + num_correction_steps)
  in
  let a_mod_p = ref a_minus_qp in
  Array.iter
    (Array.rev (Array.init num_correction_steps ~f:(fun i -> 1 lsl i)))
    ~f:(fun x ->
      let candidate = Z.(!a_mod_p - (of_int x * p)) in
      if Z.(gte candidate zero) then a_mod_p := candidate);
  if debug
  then
    print_s
      [%message
        "Intermediate values" (a' : hex_z) (q : hex_z) (qp : hex_z) (!a_mod_p : hex_z)];
  !a_mod_p
;;

let generate_z ~lo_incl ~hi_incl =
  Quickcheck.Generator.map
    ~f:Bigint.to_zarith_bigint
    (Bigint.gen_incl (Bigint.of_zarith_bigint lo_incl) (Bigint.of_zarith_bigint hi_incl))
;;

let test ?debug ~levels ~num_correction_steps a =
  let obtained = barrett_reduction ?debug ~levels ~num_correction_steps a in
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

let test_random ~levels ~num_correction_steps =
  Quickcheck.test
    (let%bind.Quickcheck.Generator a = generate_z ~lo_incl:Z.zero ~hi_incl:Z.(p - one) in
     let%bind.Quickcheck.Generator b = generate_z ~lo_incl:Z.zero ~hi_incl:Z.(p - one) in
     Quickcheck.Generator.return Z.(a * b))
    ~trials:100_000
    ~f:(test ~levels ~num_correction_steps)
;;

let%expect_test "Multiplication constants" =
  print_s [%message (m : hex_z) (p : hex_z)];
  [%expect
    {|
    ((m
      0x261508d0cc4060e976c3ca0582ef4f73bbad0de6776b1a06af2d488d85a6d02d0ed687789c42a591f9fd58c5e4daffc)
     (p
      0x1ae3a4617c510eac63b05c06ca1493b1a22d9f300f5138f1ef3622fba094800170b5d44300000008508c00000000001)) |}]
;;

let%expect_test "Quickcheck for 332 config" =
  test_random
    ~levels:Approx_msb_multiplier_model.golden_config_332
    ~num_correction_steps:3
;;

let%expect_test "Quickcheck for 2222 config" =
  test_random
    ~levels:Approx_msb_multiplier_model.golden_config_2222
    ~num_correction_steps:4
;;

(*
let%expect_test "Special values" =
  test
    ~debug:true
    ~levels:Approx_msb_multiplier_model.golden_config
    Z.((p - one) * (p - one));
  [%expect{|
    ("Intermediate values"
     (a'
      0x2ae361e5c2ce90aa50d19387f20fa27378647ec3407997c0cf3622fba094800452217cc900000000000000000000000)
     (q
      0x1ae3a4617c510eac63b05c06ca1493b1a22d9f300f5138f1ef3622fba094800170b5d44300000008508bffffffffffc)
     (qp
      0x5a3874c14ddb64a525c07f7393d1e75e91dba1331285eceb0193ba08bed6ffffffffffffffffffe70e5bffffffffffc)
     (!a_mod_p 0x1)) |}]
;;

let%expect_test "" =
  test
    ~debug:true
    ~levels:Approx_msb_multiplier_model.golden_config
    (Z.of_string
       "0x23f04030049c70b3af409d4e14d17998374719db49d47e2e06bc9941322d341174df4376adb5561bd14a16d0df7feabe089bb711aa0ae8be84588f7d94c61b839ca85c9342b57d9995bca4d2bac7254be5efd39871d55ca4d6fa019a30013");
  [%expect{|
    ("Intermediate values"
     (a'
      0x3e089bb711aa0ae8be84588f7d94c61b839ca85c9342b57d9995bca4d2bac7254be5efd39871d55ca4d6fa019a30013)
     (q
      0x156275e4da06f626ede27db2023ab34cd9eeb0d02b5600ba8e6e7f4af2038e3248e9a25a29f0c32868684817ebf818d)
     (qp
      0x6a5acce64f0912cabd20dabff35a5a7118746bfaeefe4b75c968d9244d5ee50cfee20d460e59f8c751844817ebf818d)
     (!a_mod_p
      0x302e1ac4dadcc18d65269bb2bfcb095849f5ed17650bf32028a7a8da39e6213fae265c48a17dc7c61aeb1e9ae37e83)) |}]
;;
   *)

(*
let%expect_test "Compare barret reduction vs actual mod (only radix 3)" =
  test_random ~levels:[ Radix_3 ]
;;

let%expect_test "Compare barrett reduction vs actual mod (only radix 2)" =
  test_random ~levels:[ Radix_2; Radix_2; Radix_2; Radix_2 ]
;;
   *)
