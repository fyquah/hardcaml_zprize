open Core

let sexp_of_z = Ark_bls12_377_g1.For_testing.sexp_of_z

let print_is_on_curve a =
  if Ark_bls12_377_g1.is_on_curve a
  then Stdio.printf "point is on curve!\n"
  else Stdio.printf "point is not on curve!\n"
;;

let%expect_test "Test accessors" =
  let a = Ark_bls12_377_g1.create ~x:(Z.of_int 2) ~y:(Z.of_int 3) ~infinity:true in
  Stdio.print_s ([%sexp_of: Ark_bls12_377_g1.affine] a);
  [%expect {| ((x 0x2) (y 0x3) (infinity true)) |}]
;;

let%expect_test "Print model coefficient and constants." =
  let a = Ark_bls12_377_g1.coeff_a () in
  let b = Ark_bls12_377_g1.coeff_b () in
  let modulus = Ark_bls12_377_g1.modulus () in
  Stdio.print_s [%message (a : z) (b : z) (modulus : z)];
  (* These values are cross-checked against the link below for correctness.
   * 
   * https://medium.com/asecuritysite-when-bob-met-alice/the-wonderful-bls12-curves-just-ready-for-a-privacy-respecting-world-fe731386e722
   * *)
  [%expect
    {|
    ((a 0x0) (b 0x1)
     (modulus
      0x1ae3a4617c510eac63b05c06ca1493b1a22d9f300f5138f1ef3622fba094800170b5d44300000008508c00000000001)) |}]
;;

let%expect_test "Check points that should be on the curve" =
  let create = Ark_bls12_377_g1.create in
  [ create ~x:(Z.of_int 0) ~y:(Z.of_int 0) ~infinity:true
  ; (* Infinity is on the curve *)
    create ~x:(Z.of_int 2) ~y:(Z.of_int 3) ~infinity:false
    (* LHS: 3^2 = 9
     * RHS: 2*3 + 1 = 8 + 1 = 0
     * *)
  ]
  |> List.iter ~f:(fun a -> assert (Ark_bls12_377_g1.is_on_curve a))
;;

let%expect_test "Display subgroup generator" =
  let a = Ark_bls12_377_g1.subgroup_generator () in
  let x = Ark_bls12_377_g1.x a in
  let y = Ark_bls12_377_g1.y a in
  Stdio.print_s [%message (x : z) (y : z)];
  print_is_on_curve a;
  [%expect
    {|
    ((x
      0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
     (y
      0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))
    point is on curve! |}]
;;

(* TODO(fyquah): I don't know what is the appropriate behaviour for infinity.
 * ie, should {x=1, y=2, infinity=true} and {x=123, y=345, infinity=true} be
 * equivalent values?
 *)
let%expect_test "Verify equality" =
  [ Ark_bls12_377_g1.subgroup_generator (), Ark_bls12_377_g1.subgroup_generator () ]
  |> List.iter ~f:(fun (a, b) -> assert ([%equal: Ark_bls12_377_g1.affine] a b))
;;

let%expect_test "Verify point-addition and point-multiplication give similar results." =
  let generator = Ark_bls12_377_g1.subgroup_generator () in
  for by = 1 to 10 do
    let from_mul = Ark_bls12_377_g1.mul generator ~by in
    let from_addition =
      Fn.apply_n_times ~n:(by - 1) (Ark_bls12_377_g1.add generator) generator
    in
    assert ([%equal: Ark_bls12_377_g1.affine] from_mul from_addition)
  done
;;

(* Multiplying a point by zero should yield an infinity point. *)
let%expect_test "Multiplying a point by zero" =
  let generator = Ark_bls12_377_g1.subgroup_generator () in
  print_s ([%sexp_of: Ark_bls12_377_g1.affine] (Ark_bls12_377_g1.mul ~by:0 generator));
  [%expect {| ((x 0x0) (y 0x1) (infinity true)) |}]
;;

let%expect_test "Multiply by an arbitrary [Bits.t] - allow multiplications wider than 62 \
                 bits"
  =
  let module A = Ark_bls12_377_g1 in
  let open Hardcaml in
  let rand () = A.(mul (subgroup_generator ()) ~by:(Random.int 100)) in
  let mul a b = A.mul a ~by:b in
  for _ = 1 to 100 do
    (* This tests various configurations in which [by] fits in an int. *)
    let part_width = 1 + Random.int 10 in
    let num_parts = 1 + Random.int 4 in
    let width = part_width + num_parts in
    let a = rand () in
    let b = Random.int (1 lsl width) in
    let r = mul a b in
    let w = A.mul_wide ~part_width a ~by:(Bits.of_int ~width b) in
    if not (A.equal_affine w r)
    then
      raise_s
        [%message
          "" (b : int) (part_width : int) (num_parts : int) (r : A.affine) (w : A.affine)]
  done
;;
