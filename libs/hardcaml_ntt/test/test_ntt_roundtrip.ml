(* Demonstrate the forward and inverse NTT transform roundtrips. *)

open! Core
module Gf = Hardcaml_ntt.Gf.Z
module Reference_model = Hardcaml_ntt.Reference_model.Make (Gf)

let%expect_test "round trip" =
  (* Test a few sizes, a few times. *)
  for logn = 2 to 8 do
    let n = 1 lsl logn in
    let invn = Gf.inverse (Gf.of_z (Z.of_int n)) in
    for _ = 0 to 3 do
      (* Perform a forward then inverse ntt, scale by 1/n *)
      let input = Array.init n ~f:(fun _ -> Gf.of_z (Z.of_int (Random.int 100_000))) in
      let data = Array.copy input in
      Reference_model.forward_dit data;
      Reference_model.inverse_dit data;
      let result = Array.map data ~f:(Gf.( * ) invn) in
      if not ([%compare.equal: Gf.t array] input result)
      then raise_s [%message (input : Gf.t array) (result : Gf.t array)]
    done
  done
;;
