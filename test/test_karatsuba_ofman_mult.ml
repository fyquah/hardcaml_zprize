open! Core
open! Hardcaml
open! Snarks_r_fun

(* TODO(fyquah): Add some waveform tests once I figure out how to install
 * hardcaml_waveterm on my M1 mac :)
 *)

let debug = false

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
    Sim.create ~config:Cyclesim.Config.trace_all (create scope)
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
  [%expect {| 42 |}];
  (* Demonstrate that if the enable signal is not high, the output result gets
     held.
   *)
  inputs.enable <--. 0;
  Cyclesim.cycle sim;
  print_output_as_int ();
  [%expect {| 42 |}];
  (* Now, assert [enable] - the output should get cleared with the new
     output.
   * *)
  inputs.enable <--. 1;
  Cyclesim.cycle sim;
  print_output_as_int ();
  [%expect {| 0 |}];
;;

module Triple_depth_mult = Make(struct
    let num_bits = 377
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
  let internal_ports = Cyclesim.internal_ports sim in
  let test_cases =
    List.concat [
      [ { a = Z.of_string "0x781ed5e8c458e1ba9b26ca68"
        ; b = Z.of_string "3333333333333333"
        }
      ; { a = Z.of_string "42424242424242424242424242424424"
        ; b = Z.of_string "3333333333333333"
        }
      ; { a = Z.of_string "123123012301923812098310"
        ; b = Z.of_string "43905850405824043"
        }
      ; { a = Z.of_string "1"
        ; b = Z.of_string "1"
        }
      ; { a = Ark_bls12_377_g1.x (Ark_bls12_377_g1.subgroup_generator ())
        ; b = Ark_bls12_377_g1.y (Ark_bls12_377_g1.subgroup_generator ())
        }
      ; { a = Z.of_string "0x1a317c1aef7ad5cea420c1e82e24340ab50cb36f3faab9b86f7b947029e3deac5d1d6d8dee18bb9a81f2fe9ebeaccb7"
        ; b = Z.of_string "0x112c98fab46b7e3aeafc86c5222f3edfd7cfbc8d400ce9e4c8a0cd15e4ca09e713a1c53094291e2f7ff329b8d2d289d"
        }
      ]
    ; List.init 50 ~f:(fun _ ->
          let random_bigint () = Utils.random_z ~lo_incl:Z.zero ~hi_incl:Z.((one lsl 377) - one) in
          { a = random_bigint ()
          ; b = random_bigint ()
          })
    ]
  in
  let latency = Triple_depth_mult.latency in
  let obtained_results = Queue.create () in
  let cycle () =
    Cyclesim.cycle sim;
    if Bits.is_vdd !(outputs.valid) then (
      Queue.enqueue obtained_results (Bits.to_z ~signedness:Unsigned !(outputs.c))
    );
    if debug then (
      List.iter internal_ports ~f:(fun (port_name, value) ->
          if String.is_prefix port_name ~prefix:"m2$" then
            Stdio.printf !"%s: 0x%{Sexp}\n"
              port_name
              ([%sexp_of: Utils.z] (Bits.to_z ~signedness:Unsigned !value)));
      Stdio.print_endline ""
    );
  in
  inputs.enable <--. 1;
  inputs.valid <--. 1;
  let width = Triple_depth_mult.num_bits in
  List.iter test_cases ~f:(fun { a; b } ->
      inputs.a := Bits.of_z ~width a;
      inputs.b := Bits.of_z ~width b;
      cycle ());
  inputs.a := Bits.zero width;
  inputs.b := Bits.zero width;
  inputs.valid <--. 0;
  for _ = 1 to latency do
    cycle ();
  done;
  assert (Queue.length obtained_results = (List.length test_cases));
  List.iter test_cases ~f:(fun { a; b} ->
      let expected = Z.mul a b in
      let obtained = Queue.dequeue_exn obtained_results in
      let num_bits_for_expected = Z.numbits expected in
      let result_num_bits = Triple_depth_mult.num_bits * 2 in
      if num_bits_for_expected > result_num_bits then (
        failwithf "Cannot represent result in the available num_bits, \
                   required %d, but multiplier only supports %d (a=%s, b=%s)"
          (Z.numbits expected)
          result_num_bits
          (Z.to_string a)
          (Z.to_string b)
          ()
      );
      if not (Z.equal expected obtained) then (
        let sexp_of_z z = Sexp.Atom (Z.to_string z) in
        raise_s [%message
           "Result mismatch"
             (a : z)
             (b : z)
             (expected : z)
             (obtained : z)
             (num_bits_for_expected : int)
             (result_num_bits : int)
        ]
      );
    );
  [%expect{||}]
;;
