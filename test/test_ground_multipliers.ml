open Core
open Hardcaml
open Snarks_r_fun.Karatsuba_ofman_mult.For_testing

let generate_z ~width =
  Quickcheck.Generator.map
    ~f:Bigint.to_zarith_bigint
    (Bigint.gen_incl Bigint.zero Bigint.((one lsl width) - one))
;;

type test_case =
  { big : Bits.t
  ; pivot : Bits.t
  ; rng_seed : int
  }

let test_multiplier_implementation f =
  let generate =
    let%map.Quickcheck.Generator big = generate_z ~width:24
    and pivot = generate_z ~width:7
    and rng_seed = Int.gen_incl 0 1_000_000_000 in
    { big = Bits.of_z ~width:24 big
    ; pivot = Bits.of_z ~width:7 pivot
    ; rng_seed
    }
  in
  let test_and_print a b =
    let i24 = Bits.of_int ~width:24 in
    let i7 = Bits.of_int ~width:7 in
    let result = Bits.to_int (f { big = i24 a; pivot = i7 b; rng_seed = 0 }) in
    Stdio.printf "%d * %d = %d\n\n" a b result
  in
  test_and_print 101 0;
  Quickcheck.test ~trials:100_000 generate ~f:(fun { big; pivot; rng_seed } ->
      let result =
        f { big; pivot; rng_seed }
      in
      let expected = Bits.( *: )  big pivot in
      if not (Bits.equal result expected) then (
        raise_s [%message
           "Multiplication result mismatch!"
             (big : Bits.t)
             (pivot : Bits.t)
             (result : Bits.t)
             (expected : Bits.t)
        ]
      );
    )
;;

let%expect_test "" =
  test_multiplier_implementation (fun { big; pivot; rng_seed } ->
      let rng = Random.State.make [| rng_seed |] in
      long_multiplication_with_addition
        (module Bits)
        ~is_definitely:(fun a b ->
            Random.State.bool rng && Bits.(is_vdd (a ==:. b)))
        ~pivot
        big);
  [%expect {| 101 * 0 = 0 |}]
;;

let%expect_test "" =
  test_multiplier_implementation (fun { big; pivot; rng_seed } ->
      let rng = Random.State.make [| rng_seed |] in
      long_multiplication_with_subtraction
        (module Bits)
        ~is_definitely:(fun a b ->
            Random.State.bool rng && Bits.(is_vdd (a ==:. b)))
        ~pivot
        big);
  [%expect {| 101 * 0 = 0 |}]
;;
