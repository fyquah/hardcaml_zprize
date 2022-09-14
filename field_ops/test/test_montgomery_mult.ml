open! Core
open! Hardcaml
open! Field_ops_lib

module Montgomery_mult377 = Montgomery_mult.With_interface (struct
  let bits = 377
end)

module Sim = Cyclesim.With_interface (Montgomery_mult377.I) (Montgomery_mult377.O)

let create_sim ~p ~config =
  let scope = Scope.create ~flatten_design:true () in
  Sim.create
    ~config:Cyclesim.Config.trace_all
    (Montgomery_mult377.create ~p ~config scope)
;;

type test_cases =
  { x : Z.t
  ; y : Z.t
  }

let debug = false

(* Software model of the REDC algorithm. *)
let compute_software_model ~logr ~p x y =
  let r = Z.(one lsl logr) in
  let p' =
    let { Extended_euclidean.coef_x = _; coef_y; gcd } =
      Extended_euclidean.extended_euclidean ~x:r ~y:p
    in
    assert (Z.equal gcd Z.one);
    Z.(neg coef_y mod r)
  in
  let p' = if Z.(lt p' zero) then Z.(p' + r) else p' in
  (* p'p = -1 mod r. Since we are using Zarith, having signed integers here
   * is fine. 
   *)
  assert (Z.(equal (p * p' mod r) Z.(r - one)));
  let xy = Z.(x * y) in
  if debug then Stdio.printf !"expected[xy] = %{Sexp}\n" (Utils.sexp_of_z xy);
  let m = Z.(((xy mod r * p') + r) mod r) in
  if debug then Stdio.printf !"expected[m] = %{Sexp}\n" (Utils.sexp_of_z m);
  let mp = Z.(m * p) in
  if debug then Stdio.printf !"expected[mp] = %{Sexp}\n" (Utils.sexp_of_z mp);
  let xy_plus_mp = Z.(xy + mp) in
  if debug then Stdio.printf !"expected[x + mp] = %{Sexp}\n" (Utils.sexp_of_z xy_plus_mp);
  let t = Z.(xy_plus_mp / r) in
  if debug then Stdio.printf !"expected[t] = %{Sexp}\n" (Utils.sexp_of_z t);
  let result = Z.(t mod p) in
  if Z.lt result Z.zero then Z.(result + p) else result
;;

(* The expected result of xyr' mod p *)
let compute_expected ~logr ~p x y =
  let r = Z.(one lsl logr) in
  let r' =
    let { Extended_euclidean.coef_x; coef_y = _; gcd } =
      Extended_euclidean.extended_euclidean ~x:r ~y:p
    in
    assert (Z.equal Z.one gcd);
    if Z.lt coef_x Z.zero then Z.( + ) coef_x p else coef_x
  in
  Z.(x * y * r' mod p)
;;

let p = Ark_bls12_377_g1.modulus ()
let random_bigint () = Utils.random_z ~lo_incl:Z.zero ~hi_incl:Z.(p - one)

let%expect_test _ =
  let config =
    { Montgomery_mult.Config.multiplier_config =
        `Multiplier Test_karatsuba_ofman_mult.config_four_stages
    ; montgomery_reduction_config = Montgomery_reduction.Config.for_bls12_377
    }
  in
  let test_cases =
    let hand_crafted =
      [ { x = Z.of_string "1"; y = Z.of_string "1" }
      ; { x = Z.(p - one); y = Z.(p - one) }
      ]
    in
    let auto_generated =
      List.init 50 ~f:(fun _ -> { x = random_bigint (); y = random_bigint () })
    in
    auto_generated @ hand_crafted
  in
  let sim = create_sim ~p ~config in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  if debug
  then
    Stdio.print_s
      ([%sexp_of: string list]
         (List.sort
            ~compare:String.compare
            (List.map ~f:fst (Cyclesim.internal_ports sim))));
  [%expect {| |}];
  let internal_ports = Cyclesim.internal_ports sim in
  let find_internal_port s =
    List.Assoc.find_exn internal_ports ~equal:[%equal: string] s
  in
  let find_and_print_port s =
    Stdio.printf
      !"%s = %{Sexp}\n"
      s
      (Utils.sexp_of_z (Bits.to_z ~signedness:Unsigned !(find_internal_port s)))
  in
  let is_internal_port_vdd name = Bits.is_vdd !(find_internal_port name) in
  let results = Queue.create () in
  let cycle () =
    Cyclesim.cycle sim;
    if Bits.is_vdd !(outputs.valid)
    then Queue.enqueue results (Bits.to_z ~signedness:Unsigned !(outputs.z));
    (* Debugging outputs ... *)
    if debug
    then (
      if is_internal_port_vdd "stage1$valid" then find_and_print_port "stage1$xy";
      if is_internal_port_vdd "stage2$valid"
      then (
        find_and_print_port "stage2$m";
        find_and_print_port "stage2$xy");
      if is_internal_port_vdd "stage3$valid"
      then (
        find_and_print_port "stage3$mp";
        find_and_print_port "stage3$xy");
      if is_internal_port_vdd "stage4$valid"
      then (
        find_and_print_port "stage4$t";
        find_and_print_port "stage4$xy_plus_mp");
      if is_internal_port_vdd "stage5$valid" then find_and_print_port "stage5$result")
  in
  inputs.enable := Bits.vdd;
  List.iter test_cases ~f:(fun { x; y } ->
      inputs.x := Bits.of_z ~width:Montgomery_mult377.bits x;
      inputs.y := Bits.of_z ~width:Montgomery_mult377.bits y;
      inputs.valid := Bits.vdd;
      cycle ());
  inputs.valid := Bits.gnd;
  for _ = 1 to Montgomery_mult.Config.latency config do
    cycle ()
  done;
  cycle ();
  List.map2_exn test_cases (Queue.to_list results) ~f:(fun { x; y } obtained ->
      let expected = compute_expected ~p ~logr:Montgomery_mult377.bits x y in
      if Z.equal obtained expected
      then Ok ()
      else (
        let software_model =
          compute_software_model ~p ~logr:Montgomery_mult377.bits x y
        in
        Or_error.error_s
          [%message
            "Test case failed!"
              (x : Utils.z)
              (y : Utils.z)
              (obtained : Utils.z)
              (expected : Utils.z)
              (software_model : Utils.z)]))
  |> Or_error.combine_errors_unit
  |> [%sexp_of: unit Or_error.t]
  |> Stdio.print_s;
  [%expect {|
    (Ok ()) |}]
;;
