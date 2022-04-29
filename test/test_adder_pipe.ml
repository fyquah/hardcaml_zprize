open! Core
open! Hardcaml
open! Snarks_r_fun

module Adder64 = Adder_pipe.With_interface(struct
    let bits = 64
  end)
module Sim = Cyclesim.With_interface(Adder64.I)(Adder64.O)

let create_sim ~stages =
  let scope = Scope.create ~flatten_design:true () in
  Sim.create (Adder64.create ~stages scope)
;;

let (<--.) dst src = dst := Bits.of_int ~width:(Bits.width !dst) src

type test_cases =
  { x : Z.t
  ; y : Z.t
  ; p : Z.t
  }

let sexp_of_z z = Sexp.Atom (Z.to_string z)

let%expect_test "" =
  let test_cases =
    [ { x = Z.of_string "1"
      ; y = Z.of_string "2"
      ; p = Z.of_string "42"
      }
    ; { x = Z.of_string "1"
      ; y = Z.of_string "41"
      ; p = Z.of_string "42"
      }
    ; { x = Z.of_string "1"
      ; y = Z.of_string "40"
      ; p = Z.of_string "42"
      }
    ]
  in
  let stages = 2 in
  let sim = create_sim ~stages in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let results = Queue.create () in
  let cycle () =
    Cyclesim.cycle sim;
    if Bits.is_vdd !(outputs.valid) then (
      Queue.enqueue results (Bits.to_z ~signedness:Unsigned !(outputs.z))
    );
  in
  inputs.enable <--. 1;
  List.iter test_cases ~f:(fun { x; y; p } ->
      inputs.p := Bits.of_z ~width:Adder64.bits p;
      inputs.x := Bits.of_z ~width:Adder64.bits x;
      inputs.y := Bits.of_z ~width:Adder64.bits y;
      inputs.valid <--. 1;
      cycle ();
    );
  inputs.valid <--. 0;
  for _ = 1 to Adder_pipe.latency ~stages do
    cycle ();
  done;
  cycle ();
  List.map2_exn test_cases (Queue.to_list results) ~f:(fun { x; y; p } obtained ->
      let expected = Z.((x + y) mod p) in
      if Z.equal obtained expected
      then Ok ()
      else Or_error.error_s [%message
           "Test case failed!"
             (x : z)
             (y : z)
             (obtained : z)
             (expected : z)
        ])
  |> Or_error.combine_errors_unit
  |> [%sexp_of: unit Or_error.t]
  |> Stdio.print_s;
  [%expect {| (Ok ()) |}]
;;
