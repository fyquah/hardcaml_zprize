open! Base
open! Hardcaml
open! Field_ops_lib
open! Bits

module Barrett_reduction377 = Barrett_reduction.With_interface (struct
  let bits = 377
  let output_bits = 377
end)

module Config = Barrett_reduction.Config
module I = Barrett_reduction377.I
module O = Barrett_reduction377.O
module Sim = Cyclesim.With_interface (I) (O)

let create_sim ~p ~config =
  let scope = Scope.create ~flatten_design:true () in
  Sim.create
    ~config:{ Cyclesim.Config.trace_all with deduplicate_signals = false }
    (Barrett_reduction377.create ~config ~p scope)
;;

let compute_software_model ~p ~a =
  let k = 2 * Z.log2up p in
  let m = Z.((one lsl k) / p) in
  let q = Z.((a * m) asr k) in
  let qp = Z.(q * p) in
  let a_minus_qp = Z.(a - qp) in
  if Z.gt a_minus_qp Z.(p - one) then Z.(a_minus_qp - p) else a_minus_qp
;;

let config = Barrett_reduction.Config.for_bls12_377
let p = Ark_bls12_377_g1.modulus ()
let random_bigint () = Utils.random_z ~lo_incl:Z.zero ~hi_incl:Z.((p - one) * (p - one))

let test ~debug test_cases =
  let sim = create_sim ~p ~config in
  let internal_ports = Cyclesim.internal_ports sim in
  let dump_stage_if_valid prefix =
    if debug
    then (
      let valid_port_name = prefix ^ "$valid" in
      let valid =
        List.Assoc.find_exn ~equal:String.equal internal_ports valid_port_name
      in
      if Bits.is_vdd !valid
      then
        List.filter internal_ports ~f:(fun (key, _) ->
          String.is_prefix ~prefix key && not (String.equal valid_port_name key))
        |> List.iter ~f:(fun (port_name, value) ->
             Stdio.printf
               "%s: 0x%s\n"
               port_name
               (Z.format "x" (Bits.to_z ~signedness:Unsigned !value))))
  in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let queue = Queue.create () in
  inputs.enable := vdd;
  let cycle () =
    Cyclesim.cycle sim;
    if is_vdd !(outputs.valid)
    then Queue.enqueue queue (Bits.to_z ~signedness:Unsigned !(outputs.a_mod_p));
    dump_stage_if_valid "stage1";
    dump_stage_if_valid "stage2";
    dump_stage_if_valid "stage3";
    dump_stage_if_valid "stage4"
  in
  List.iter test_cases ~f:(fun test_case ->
    inputs.valid := vdd;
    inputs.a := Bits.of_z ~width:754 test_case;
    cycle ());
  inputs.valid := gnd;
  for _ = 1 to Config.latency config + 1 do
    cycle ()
  done;
  List.map2_exn test_cases (Queue.to_list queue) ~f:(fun a obtained ->
    let expected = Z.(a mod p) in
    let from_software_model = compute_software_model ~p ~a in
    if Z.equal obtained expected
    then Ok ()
    else
      Or_error.error_s
        [%message
          (a : Utils.z)
            (obtained : Utils.z)
            (expected : Utils.z)
            (from_software_model : Utils.z)])
  |> Or_error.combine_errors_unit
  |> [%sexp_of: unit Or_error.t]
  |> Stdio.print_s
;;

let%expect_test "Randomized test" =
  let test_cases =
    List.concat
      [ [ Z.zero
        ; Z.one
        ; Z.of_int 2
        ; Z.((p - one) * (p - one))
        ; Z.of_string
            "0x1c1d96627014caa774cc857b5e15be00056047380b0e58c8d3a0a1cae6ca8b12a578e6418715d6078973059922e91c52e73fb7ebb5114a2e53da4dc61bf355481fb0deb8c1520f806159b6eb6a79b5e62ba2b87ad0baa6c1a6b262627243b"
        ]
      ; List.init 1_000 ~f:(fun _ -> random_bigint ())
      ]
  in
  test ~debug:false test_cases;
  [%expect {| (Ok ()) |}]
;;

let%expect_test "regression test case" =
  test
    ~debug:true
    [ Z.of_string
        "0x23f04030049c70b3af409d4e14d17998374719db49d47e2e06bc9941322d341174df4376adb5561bd14a16d0df7feabe089bb711aa0ae8be84588f7d94c61b839ca85c9342b57d9995bca4d2bac7254be5efd39871d55ca4d6fa019a30013"
    ];
  [%expect
    {|
    stage1$q: 0x156275e4da06f626ede27db2023ab34cd9eeb0d02b5600ba8e6e7f4af2038e3248e9a25a29f0c32868684817ebf818b
    stage1$a': 0xbe089bb711aa0ae8be84588f7d94c61b839ca85c9342b57d9995bca4d2bac7254be5efd39871d55ca4d6fa019a30013
    stage2$qp: 0x349384235666f571f5c022b25f31330dd4192d9ad05bd991eafc932d0c35e50a1d7664c00e59f8b6b06c4817ebf818b
    stage2$a': 0xbe089bb711aa0ae8be84588f7d94c61b839ca85c9342b57d9995bca4d2bac7254be5efd39871d55ca4d6fa019a30013
    stage3$a_minus_qp: 0x89751793bb431576c8c435dd1e63930daf837ac1c2e6dbebae992977c684e21b2e6f8b138a17dca5f46ab1e9ae37e88
    stage4$a_mod_p: 0x302e1ac4dadcc18d65269bb2bfcb095849f5ed17650bf32028a7a8da39e6213fae265c48a17dc7c61aeb1e9ae37e83
    (Ok ()) |}]
;;
