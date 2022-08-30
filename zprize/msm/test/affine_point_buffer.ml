open Core

(* TODO(fyquah): All this bit banging should be in C *)

type t = bytes

let of_affine_points (affine_points : Ark_bls12_377_g1.affine list) =
  let num_points = List.length affine_points in
  let dst = Bytes.init (num_points * 96) ~f:(fun _ -> '\000') in
  List.iteri affine_points ~f:(fun i point ->
      let x = Z.to_bits (Ark_bls12_377_g1.x point) in
      let y = Z.to_bits (Ark_bls12_377_g1.y point) in
      let is_infinity = Bool.to_int (Ark_bls12_377_g1.infinity point) in
      let base_pos = i * 96 in
      Bytes.From_string.blito ~src:x ~dst ~dst_pos:base_pos ();
      Bytes.From_string.blito ~src:y ~dst ~dst_pos:(base_pos + 48) ();
      let last_byte_pos = (i * 96) + 95 in
      let last_byte = Char.to_int (Bytes.get dst last_byte_pos) lor (is_infinity lsl 1) in
      Bytes.set dst last_byte_pos (Char.unsafe_of_int last_byte));
  dst
;;

let to_affine_points buffer =
  assert (Bytes.length buffer % 96 = 0);
  let num_points = Bytes.length buffer / 96 in
  List.init num_points ~f:(fun i ->
      let base_pos = i * 96 in
      let last_byte_pos = base_pos + 95 in
      let last_byte = Char.to_int (Bytes.get buffer last_byte_pos) in
      let x =
        let x = Bytes.subo ~pos:base_pos ~len:48 buffer in
        Z.of_bits (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:x)
      in
      let y =
        let y = Bytes.subo ~pos:(base_pos + 48) ~len:48 buffer in
        Bytes.set y 47 (Char.unsafe_of_int (last_byte land 1));
        Z.of_bits (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:y)
      in
      let infinity =
        match (last_byte lsr 1) land 1 with
        | 0 -> false
        | 1 -> true
        | _ -> assert false
      in
      Ark_bls12_377_g1.create ~x ~y ~infinity)
;;

let sexp_of_t = Bytes.Hexdump.sexp_of_t

let%expect_test "" =
  let generate =
    let generate_z =
      Field_ops_test.Utils.generate_z
        ~lo_incl:Z.zero
        ~hi_incl:Z.(sub (Ark_bls12_377_g1.modulus ()) one)
    in
    let generate_point =
      let%bind.Quickcheck.Generator x = generate_z in
      let%bind.Quickcheck.Generator y = generate_z in
      let%map.Quickcheck.Generator infinity = Bool.quickcheck_generator in
      Ark_bls12_377_g1.create ~x ~y ~infinity
    in
    let%bind.Quickcheck.Generator len = Int.gen_incl 1 20 in
    List.gen_with_length len generate_point
  in
  Quickcheck.test ~trials:200 generate ~f:(fun points ->
      assert (
        [%equal: Ark_bls12_377_g1.affine list]
          points
          (to_affine_points (of_affine_points points))))
;;

let%expect_test "" =
  let p = Ark_bls12_377_g1.create ~x:(Z.of_int 2) ~y:(Z.of_int 3) ~infinity:true in
  let buffer = of_affine_points [ p ] in
  print_s [%message (buffer : t)];
  [%expect
    {|
    (buffer
     ("00000000  02 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
      "00000010  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
      "00000020  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
      "00000030  03 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
      "00000040  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
      "00000050  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 02  |................|")) |}]
;;
