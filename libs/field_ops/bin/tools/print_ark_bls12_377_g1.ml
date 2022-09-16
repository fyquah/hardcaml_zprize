(** A binary to display the outputs from using the ark_bls12_377_g1
    library. This is used primarily to debug the rust->OCaml bindings.
*)

open Core
open Field_ops_test

type affine = Ark_bls12_377_g1.affine [@@deriving sexp_of, equal]

let coeff_a = Ark_bls12_377_g1.coeff_a ()
let coeff_b = Ark_bls12_377_g1.coeff_b ()

let () =
  Stdio.printf !"a = 0x%{Sexp}\n" (Utils.sexp_of_z coeff_a);
  Stdio.printf !"b = 0x%{Sexp}\n" (Utils.sexp_of_z coeff_b);
  let subgroup_generator = Ark_bls12_377_g1.subgroup_generator () in
  Stdio.print_s [%message (subgroup_generator : affine)];
  let manually_constructed =
    Ark_bls12_377_g1.create
      ~x:
        (Z.of_string_base
           16
           "8848DEFE740A67C8FC6225BF87FF5485951E2CAA9D41BB188282C8BD37CB5CD5481512FFCD394EEAB9B16EB21BE9EF")
      ~y:
        (Z.of_string_base
           16
           "1914A69C5102EFF1F674F5D30AFEEC4BD7FB348CA3E52D96D182AD44FB82305C2FE3D3634A9591AFD82DE55559C8EA6")
      ~infinity:false
  in
  Stdio.print_s [%message (manually_constructed : affine)];
  Stdio.printf
    "is manually constructed and obtained from subgroup_generator equaivalent = %b\n"
    ([%equal: affine] subgroup_generator manually_constructed)
;;
