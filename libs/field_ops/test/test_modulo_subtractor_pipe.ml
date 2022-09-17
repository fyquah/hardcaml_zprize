(* TODO(fquah): Share code below with test_adder_pipe. *)

open! Core
open! Hardcaml
open! Field_ops_lib

module Subtracter377 = Modulo_subtractor_pipe.With_interface (struct
  let bits = 377
end)

module Sim = Cyclesim.With_interface (Subtracter377.I) (Subtracter377.O)

let p = Ark_bls12_377_g1.modulus ()

let create_sim ~stages =
  let scope = Scope.create ~flatten_design:true () in
  Sim.create (Subtracter377.create ~p ~stages scope)
;;

let ( <--. ) dst src = dst := Bits.of_int ~width:(Bits.width !dst) src

type test_cases =
  { x : Z.t
  ; y : Z.t
  }

let sexp_of_z z = Sexp.Atom (Z.to_string z)

let%expect_test "" =
  let test_cases =
    List.concat
      [ [ { x = Z.of_string "1"; y = Z.of_string "1" }
        ; { x = Z.of_string "0"; y = Z.of_string "1" }
        ; { x =
              Z.of_string
                "0xe0e256ba5aa3f3e40c3639e906da2f99eab99d553da9f7400d68ce6405912813bae37f66044adfb1290b1cb3e03cdb"
          ; y =
              Z.of_string
                "0x17c530d26d8fcfafb97738e03852f7cb69097c78231eee81276be2cbfc23889b98c24708cc1b8fd3e32988126b3143f"
          }
        ]
      ; List.init 50 ~f:(fun _ ->
            let random_bigint () = Utils.random_z ~lo_incl:Z.zero ~hi_incl:Z.(p - one) in
            { x = random_bigint (); y = random_bigint () })
      ]
  in
  let stages = 3 in
  let sim = create_sim ~stages in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let results = Queue.create () in
  let cycle () =
    Cyclesim.cycle sim;
    if Bits.is_vdd !(outputs.valid)
    then Queue.enqueue results (Bits.to_z ~signedness:Unsigned !(outputs.z))
  in
  inputs.enable <--. 1;
  List.iter test_cases ~f:(fun { x; y } ->
      inputs.x := Bits.of_z ~width:Subtracter377.bits x;
      inputs.y := Bits.of_z ~width:Subtracter377.bits y;
      inputs.valid <--. 1;
      cycle ());
  inputs.valid <--. 0;
  for _ = 1 to Modulo_subtractor_pipe.latency ~stages do
    cycle ()
  done;
  cycle ();
  List.map2_exn test_cases (Queue.to_list results) ~f:(fun { x; y } obtained ->
      let expected = Z.((x - y + p) mod p) in
      if Z.equal obtained expected
      then Ok ()
      else
        Or_error.error_s
          [%message "Test case failed!" (x : z) (y : z) (obtained : z) (expected : z)])
  |> Or_error.combine_errors_unit
  |> [%sexp_of: unit Or_error.t]
  |> Stdio.print_s;
  [%expect {| (Ok ()) |}]
;;
