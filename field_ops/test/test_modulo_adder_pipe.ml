open! Core
open! Hardcaml
open! Field_ops_lib

module Adder377 = Modulo_adder_pipe.With_interface (struct
  let bits = 377
end)

module Sim = Cyclesim.With_interface (Adder377.I) (Adder377.O)

let p = Ark_bls12_377_g1.modulus ()

let create_sim ~stages =
  let scope = Scope.create ~flatten_design:true () in
  Sim.create (Adder377.create ~p ~stages scope)
;;

let ( <--. ) dst src = dst := Bits.of_int ~width:(Bits.width !dst) src

type test_cases =
  { x : Z.t
  ; y : Z.t
  }

let sexp_of_z z = Sexp.Atom (Z.to_string z)
let random_bigint () = Utils.random_z ~lo_incl:Z.zero ~hi_incl:Z.(p - one)

let%expect_test "" =
  let test_cases =
    let hand_crafted =
      [ { x = Z.of_string "1"; y = Z.of_string "2" }
      ; { x = Z.of_string "1"; y = Z.of_string "41" }
      ; { x = Z.of_string "1"; y = Z.of_string "40" }
      ]
    in
    hand_crafted
    @ List.init 50 ~f:(fun _ -> { x = random_bigint (); y = random_bigint () })
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
      inputs.x := Bits.of_z ~width:Adder377.bits x;
      inputs.y := Bits.of_z ~width:Adder377.bits y;
      inputs.valid <--. 1;
      cycle ());
  inputs.valid <--. 0;
  for _ = 1 to Modulo_adder_pipe.latency ~stages do
    cycle ()
  done;
  cycle ();
  List.map2_exn test_cases (Queue.to_list results) ~f:(fun { x; y } obtained ->
      let expected = Z.((x + y) mod p) in
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
