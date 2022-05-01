open! Base
open! Hardcaml
open! Snarks_r_fun

module Montgomery_mult256 = Montgomery_mult.With_interface(struct
    let bits = 256
  end)
module Sim = Cyclesim.With_interface(Montgomery_mult256.I)(Montgomery_mult256.O)

let create_sim ~p ~config =
  let scope = Scope.create ~flatten_design:true () in
  Sim.create ~config:Cyclesim.Config.trace_all
    (Montgomery_mult256.create ~p ~config scope)
;;

type test_cases =
  { x : Z.t
  ; y : Z.t
  }

(*
let compute_expected ~p x y =
  let logr = Z.log2up p in
  let r = Z.(one lsl logr) in
  let r' =
    let { Extended_euclidean. coef_x; coef_y = _; gcd } =
      Extended_euclidean.extended_euclidean ~x:r ~y:p
    in
    assert (Z.equal gcd Z.one);
    coef_x
  in
  let result = Z.((x * y * r') mod p) in
  if Z.(lt result zero) then
    Z.(result + p)
  else
    result
;;
   *)

let debug = false

(* Softwar model of the REDC algorithm. *)
let compute_software_model ~logr ~p x y =
  let r = Z.(one lsl logr) in
  let p' =
    let { Extended_euclidean. coef_x = _ ; coef_y; gcd } =
      Extended_euclidean.extended_euclidean ~x:r ~y:p
    in
    assert (Z.equal gcd Z.one);
    Z.neg coef_y
  in
  (* p'p = -1 mod r. Since we are using Zarith, having signed integers here
   * is fine. 
   *)
  assert Z.(equal ((p * p') mod r) (neg one));
  let xy = Z.(x * y) in
  let m = Z.((((xy mod r) * p') + r) mod r) in
  if debug then (
    Stdio.printf !"expected[m] = %{Z}\n" m;
  );
  let mp = Z.(m * p) in
  if debug then (
    Stdio.printf !"expected[mp] = %{Z}\n" mp;
  );
  let t = Z.((xy + mp) / r) in
  if debug then (
    Stdio.printf !"expected[t] = %{Z}\n" t;
  );
  let result = Z.(t mod p) in
  if Z.lt result Z.zero then
    Z.(result + p)
  else
    result
;;

(* The expected result of xyr' mod p *)
let compute_expected ~logr ~p x y =
  let r = Z.(one lsl logr) in
  let r' =
    let { Extended_euclidean. coef_x ; coef_y = _; gcd } =
      Extended_euclidean.extended_euclidean ~x:r ~y:p
    in
    assert (Z.equal Z.one gcd);
    if Z.lt coef_x Z.zero then
      Z.(+) coef_x p
    else
      coef_x
  in
  Z.((x * y * r') mod p)
;;

let%expect_test _ =
  let p = Utils.a_big_prime in
  let config =
    { Montgomery_mult.Config.
      multiplier_depth = 3
    ; adder_depth = 1
    ; subtracter_depth = 1
    }
  in
  let test_cases =
    [ { x = Z.of_string "1"
      ; y = Z.of_string "1"
      }
    ; { x = Z.(p - one)
      ; y = Z.(p - one)
      }
    ]
  in
  let sim = create_sim ~p ~config in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  Stdio.print_s (
    [%sexp_of: string list]
      (List.sort ~compare:String.compare (List.map ~f:fst (Cyclesim.internal_ports sim))));
  [%expect {|
    (gnd stage1$valid stage1$xy stage2$m stage2$valid stage2$xy stage3$mp
     stage3$valid stage3$xy stage4$t stage4$valid stage5$result stage5$valid) |}];
  let internal_ports = Cyclesim.internal_ports sim in
  let find_internal_port s = List.Assoc.find_exn internal_ports ~equal:[%equal: string] s in
  let find_and_print_port s =
    Stdio.printf !"%s = %{Z}\n"
      s
      (Bits.to_z ~signedness:Unsigned !(find_internal_port s))
  in
  let is_internal_port_vdd name = Bits.is_vdd !(find_internal_port name) in
  let results = Queue.create () in
  let cycle () =
    Cyclesim.cycle sim;
    if Bits.is_vdd !(outputs.valid) then (
      Queue.enqueue results (Bits.to_z ~signedness:Unsigned !(outputs.z))
    );
    (* Debugging outputs ... *)
    if debug then (
      if is_internal_port_vdd "stage1$valid" then (
        find_and_print_port "stage1$xy";
      );
      if is_internal_port_vdd "stage2$valid" then (
        find_and_print_port "stage2$m";
        find_and_print_port "stage2$xy";
      );
      if is_internal_port_vdd "stage3$valid" then (
        find_and_print_port "stage3$mp";
        find_and_print_port "stage3$xy";
      );
      if is_internal_port_vdd "stage4$valid" then (
        find_and_print_port "stage4$t";
      );
      if is_internal_port_vdd "stage5$valid" then (
        find_and_print_port "stage5$result";
      );
    )
  in
  inputs.enable := Bits.vdd;
  List.iter test_cases ~f:(fun { x; y } ->
      inputs.x := Bits.of_z ~width:Montgomery_mult256.bits x;
      inputs.y := Bits.of_z ~width:Montgomery_mult256.bits y;
      inputs.valid := Bits.vdd;
      cycle ();
    );
  inputs.valid := Bits.gnd;
  for _ = 1 to Montgomery_mult.Config.latency config do
    cycle ();
  done;
  cycle ();
  List.map2_exn test_cases (Queue.to_list results)
    ~f:(fun { x; y } obtained ->
        let expected = compute_expected ~p ~logr:Montgomery_mult256.bits x y in
        if Z.equal obtained expected
        then Ok ()
        else
          let software_model = compute_software_model ~p ~logr:Montgomery_mult256.bits x y in
          Or_error.error_s [%message
            "Test case failed!"
              (x : Utils.z)
              (y : Utils.z)
              (obtained : Utils.z)
              (expected : Utils.z)
              (software_model : Utils.z)
          ])
  |> Or_error.combine_errors_unit
  |> [%sexp_of: unit Or_error.t]
  |> Stdio.print_s;
  [%expect {| (Ok ()) |}]
;;
