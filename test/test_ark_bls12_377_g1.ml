open Core

let sexp_of_z a =
  Sexp.Atom ("0x" ^ Z.format "X" a)
;;

let print_is_on_curve a =
  if Ark_bls12_377_g1.is_on_curve a then
    Stdio.printf "point is on curve!\n"
  else
    Stdio.printf "point is not on curve!\n"
;;

let%expect_test "Test accessors" =
  let a =
    Ark_bls12_377_g1.create ~x:(Z.of_int 2) ~y:(Z.of_int 3) ~infinity:true
  in
  Stdio.print_s ([%sexp_of: Ark_bls12_377_g1.affine] a);
  [%expect {| ((x 2) (y 3) (infinity true)) |}];
;;

let%expect_test "Print model coefficient and constants." =
  let a = Ark_bls12_377_g1.coeff_a () in
  let b = Ark_bls12_377_g1.coeff_b () in
  let modulus = Ark_bls12_377_g1.modulus () in
  Stdio.print_s [%message (a : z) (b : z) (modulus : z)];
  (* These values are cross-checked against the link below for correctness.
   * 
   * https://medium.com/asecuritysite-when-bob-met-alice/the-wonderful-bls12-curves-just-ready-for-a-privacy-respecting-world-fe731386e722
   **)
  [%expect {|
    ((a 0x0) (b 0x1)
     (modulus
      0x1AE3A4617C510EAC63B05C06CA1493B1A22D9F300F5138F1EF3622FBA094800170B5D44300000008508C00000000001)) |}]
;;

let%expect_test "Check points that should be on the curve" =
  let create = Ark_bls12_377_g1.create in
  [ create ~x:(Z.of_int 0) ~y:(Z.of_int 0) ~infinity:true;
    (* Infinity is on the curve *)

    create ~x:(Z.of_int 2) ~y:(Z.of_int 3) ~infinity:false;
    (* LHS: 3^2 = 9
     * RHS: 2*3 + 1 = 8 + 1 = 0
     **)
  ]
  |> List.iter ~f:(fun a -> assert (Ark_bls12_377_g1.is_on_curve a))
;;

let%expect_test "Display subgroup generator" =
  let a = Ark_bls12_377_g1.subgroup_generator () in
  let x = "0x" ^ Z.format "X" (Ark_bls12_377_g1.x a) in
  let y = "0x" ^ Z.format "X" (Ark_bls12_377_g1.y a) in
  Stdio.print_s ([%message x y]);
  print_is_on_curve a;
  [%expect {|
    (0x8848DEFE740A67C8FC6225BF87FF5485951E2CAA9D41BB188282C8BD37CB5CD5481512FFCD394EEAB9B16EB21BE9EF
     0x1914A69C5102EFF1F674F5D30AFEEC4BD7FB348CA3E52D96D182AD44FB82305C2FE3D3634A9591AFD82DE55559C8EA6)
    point is on curve! |}]
;;

(* TODO(fyquah): I don't know what is the appropriate behaviour for infinity.
 * ie, should {x=1, y=2, infinity=true} and {x=123, y=345, infinity=true} be
 * equivalent values?
*)
let%expect_test "Verify equality" =
  [ ( Ark_bls12_377_g1.subgroup_generator (),
      Ark_bls12_377_g1.subgroup_generator () )
  ]
  |> List.iter ~f:(fun (a, b) ->
      assert ([%equal: Ark_bls12_377_g1.affine] a b))
;;

let%expect_test "Verify point-addition and point-multiplication give similar \
                 results." =
  let generator = Ark_bls12_377_g1.subgroup_generator () in
  for by = 1 to 10 do
    let from_mul = Ark_bls12_377_g1.mul generator ~by in
    let from_addition =
      Fn.apply_n_times ~n:(by - 1)
        (Ark_bls12_377_g1.add generator)
        generator
    in
    assert ([%equal: Ark_bls12_377_g1.affine] from_mul from_addition)
  done;
;;

(* Multiplying a point by zero should yield an infinity point. *)
let%expect_test "Multiplying a point by zero" =
  let generator = Ark_bls12_377_g1.subgroup_generator () in
  print_s ([%sexp_of: Ark_bls12_377_g1.affine]
             (Ark_bls12_377_g1.mul ~by:0 generator));
  [%expect {| ((x 0) (y 1) (infinity true)) |}]
;;
