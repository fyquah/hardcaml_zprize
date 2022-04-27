open! Core
open! Hardcaml
open! Snarks_r_fun

(* TODO(fyquah): Add some waveform tests once I figure out how to install
 * hardcaml_waveterm on my M1 mac :)
 *)

module Make(M : sig
    val num_bits : int
    val depth : int
  end) = struct
  include M
  include Karatsuba_ofman_mult.With_interface(M)

  let latency = Karatsuba_ofman_mult.latency ~depth

  let create_sim () =
    let scope = Scope.create ~flatten_design:true () in
    let module Sim = Cyclesim.With_interface(I)(O) in
    Sim.create (create scope)
  ;;
end

module Single_depth_mult = Make(struct
    let num_bits = 32
    let depth = 1
  end)

let (<--.) dst src = dst := Bits.of_int ~width:(Bits.width !dst) src

let%expect_test "Demonstrate pipelining and control signals" =
  let sim = Single_depth_mult.create_sim () in
  let latency = Karatsuba_ofman_mult.latency ~depth:1 in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  inputs.enable <--. 1;
  inputs.a <--. 7;
  inputs.b <--. 6;
  for _ = 1 to latency do
    Cyclesim.cycle sim;
    inputs.a <--. 0;
    inputs.b <--. 0;
  done;
  let print_output_as_int () =
    Stdio.printf "%d\n" (Bits.to_int !(outputs.c))
  in
  print_output_as_int ();
  [%expect {| 281474976710698 |}];
  (* Demonstrate that if the enable signal is not high, the output result gets
     held.
   *)
  inputs.enable <--. 0;
  Cyclesim.cycle sim;
  print_output_as_int ();
  [%expect {| 281474976710698 |}];
  (* Now, assert [enable] - the output should get cleared with the new
     output.
   * *)
  inputs.enable <--. 1;
  Cyclesim.cycle sim;
  print_output_as_int ();
  [%expect {| 0 |}];
;;

module Triple_depth_mult = Make(struct
    let num_bits = 200
    let depth = 3
  end)

type test_case =
  { a : Z.t
  ; b : Z.t
  }

let%expect_test "Large multiplier" =
  let sim = Triple_depth_mult.create_sim () in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let test_cases =
    [ { a = Z.of_string "-1"
      ; b = Z.of_string "-1"
      }
    ; { a = Z.of_string "-1"
      ; b = Z.of_string "1"
      }
    ; { a = Z.of_string "-42424242424242424242424242424424"
      ; b = Z.of_string "3333333333333333"
      }
    ; { a = Z.of_string "123123012301923812098310"
      ; b = Z.of_string "43905850405824043"
      }
    ]
  in
  let latency = Triple_depth_mult.latency in
  let obtained_results = Queue.create () in
  let cycle () =
    Cyclesim.cycle sim;
    if Bits.is_vdd !(outputs.valid) then (
      Queue.enqueue obtained_results (Bits.to_z ~signedness:Signed !(outputs.c))
    )
  in
  inputs.enable <--. 1;
  inputs.valid <--. 1;
  List.iter test_cases ~f:(fun { a; b } ->
      let width = Triple_depth_mult.num_bits in
      inputs.a := Bits.of_z ~width a;
      inputs.b := Bits.of_z ~width b;
      cycle ());
  inputs.valid <--. 0;
  for _ = 1 to latency do
    cycle ();
  done;
  assert (Queue.length obtained_results = (List.length test_cases));
  List.iter test_cases ~f:(fun { a; b} ->
      let expected = Z.mul a b in
      let obtained = Queue.dequeue_exn obtained_results in
      if (Z.numbits expected > Triple_depth_mult.num_bits) then (
        failwithf "Cannot represent result in the available num_bits, \
                   required %d, but multiplier only supports %d"
          (Z.numbits expected)
          Triple_depth_mult.num_bits
          ()
      );
      if not (Z.equal expected obtained) then (
        failwithf !"Result mismatched! a=%{Z} b=%{Z} expected=%{Z} obtained=%{Z}"
          a
          b
          expected
          obtained
          ()
      );
    );
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure
    "Result mismatched! a=-1 b=-1 expected=1 obtained=60708411000593732245582638661532304239961688072875817708543564698364471198454793080")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Stdlib__List.iter in file "list.ml", line 110, characters 12-15
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]
;;
