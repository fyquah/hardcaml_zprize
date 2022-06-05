open! Base
open! Hardcaml
open! Snarks_r_fun
open! Bits

let debug = false

module Barrett_reduction256 = Barrett_reduction.With_interface(struct
    let bits = 256
  end)
module Config = Barrett_reduction.Config
module I = Barrett_reduction256.I
module O = Barrett_reduction256.O
module Sim = Cyclesim.With_interface(I)(O)

let create_sim ~p ~config =
  let scope = Scope.create ~flatten_design:true () in
  Sim.create
    ~config:Cyclesim.Config.trace_all
    (Barrett_reduction256.create ~config ~p scope)
;;

let compute_software_model ~p ~a =
  let k = 2 * (Z.log2up p) in
  let m = Z.((one lsl k) / p) in
  let q = Z.((a * m) asr k) in
  let qp = Z.(q * p) in
  let a_minus_qp = Z.(a - qp) in
  if debug then (
    Stdio.print_s [%message
       (m : Utils.z)
       (q : Utils.z)
       (qp : Utils.z)
       (a_minus_qp : Utils.z)
    ];
  );
  if Z.gt a_minus_qp Z.(p - one) then
    Z.(a_minus_qp - p)
  else
    a_minus_qp
;;

let%expect_test _ =
  let config =
    { Config.
      multiplier_depth = 2
    ; subtracter_stages = 2
    }
  in
  let p = Utils.a_big_prime in
  Stdio.print_s [%message (p : Utils.z)];
  [%expect {|
    (p
     21888242871839275222246405745257275088696311157297823662689037894645226208583) |}];
  let test_cases =
    [ Z.((p - one) * (p - one))
    ]
  in
  let sim = create_sim ~p ~config in
  let internal_ports = Cyclesim.internal_ports sim in
  if debug then (
    Stdio.print_s ([%sexp_of: string list] (List.map ~f:fst internal_ports))
  );
  [%expect {| |}];
  let dump_stage_if_valid prefix =
    if debug then
      let valid_port_name = (prefix ^ "$valid") in
      let valid =
        List.Assoc.find_exn ~equal:String.equal internal_ports valid_port_name
      in
      if Bits.is_vdd !valid then (
        List.filter internal_ports ~f:(fun (key, _) ->
            String.is_prefix ~prefix key && not (String.equal valid_port_name key))
        |> List.iter ~f:(fun (port_name, value) ->
            Stdio.printf "%s: %s\n"
              port_name
              (Z.to_string (Bits.to_z ~signedness:Unsigned !value)))
      );
  in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let queue = Queue.create () in
  inputs.enable := vdd;
  let cycle () =
    Cyclesim.cycle sim;
    if is_vdd !(outputs.valid) then (
      Queue.enqueue queue (Bits.to_z ~signedness:Unsigned !(outputs.a_mod_p))
    );
    dump_stage_if_valid "stage1";
    dump_stage_if_valid "stage2";
    dump_stage_if_valid "stage3";
    dump_stage_if_valid "stage4";
  in
  List.iter test_cases ~f:(fun test_case ->
      inputs.valid := vdd;
      inputs.a := Bits.of_z ~width:512 test_case;
      cycle ();
    );
  inputs.valid := gnd;
  for _ = 1 to Config.latency config + 1 do
    cycle ();
  done;
  List.map2_exn test_cases (Queue.to_list queue) ~f:(fun a obtained ->
      let expected = Z.(a mod p) in
      let from_software_model = compute_software_model ~p ~a in
      if Z.equal obtained expected then
        Ok ()
      else
        Or_error.error_s [%message
           (a : Utils.z)
           (obtained : Utils.z)
           (expected : Utils.z)
           (from_software_model : Utils.z)
        ]
    )
  |> Or_error.combine_errors_unit
  |> [%sexp_of: unit Or_error.t]
  |> Stdio.print_s;
  [%expect {| (Ok ()) |}]
;;
