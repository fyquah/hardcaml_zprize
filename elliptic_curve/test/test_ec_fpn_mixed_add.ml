open Core
open Hardcaml
open Elliptic_curve_lib
open Point
module Config = Ec_fpn_mixed_add.Config

let () = Caller_id.set_mode Full_trace

module Ec_fpn_mixed_add = Ec_fpn_mixed_add.With_interface (struct
  let bits = 377
end)

module Sim = Cyclesim.With_interface (Ec_fpn_mixed_add.I) (Ec_fpn_mixed_add.O)

let p = Ark_bls12_377_g1.modulus ()
let ( <--. ) dst src = dst := Bits.of_int ~width:(Bits.width !dst) src

let create_sim config =
  let scope = Scope.create ~flatten_design:true () in
  let is_internal_port x =
    List.exists (Signal.names x) ~f:(fun name ->
        (not (String.is_substring name ~substring:"karatsuba_ofman_mult"))
        && not (String.is_substring name ~substring:"barrett_reduction"))
  in
  Sim.create
    ~config:{ Cyclesim.Config.default with is_internal_port = Some is_internal_port }
    (Ec_fpn_mixed_add.create scope ~config)
;;

let modulo_inverse x = Utils.modulo_inverse ~p x
let jacobian_to_affine = Utils.jacobian_to_affine p
let affine_to_jacobian = Utils.affine_to_jacobian p
let modulo_multiply a b = Utils.modulo_multiply ~p a b
let c_R = Z.(one lsl log2up p)
let c_R' = modulo_inverse c_R
let transform_to_montgomery a = Z.(a * c_R mod p)
let transform_from_montgomery a = Z.(a * c_R' mod p)

let compute_expected ~jacobian ~(affine : Z.t Affine.t) =
  let affine = Ark_bls12_377_g1.create ~x:affine.x ~y:affine.y ~infinity:false in
  Ark_bls12_377_g1.add (jacobian_to_affine jacobian) affine
;;

let test ?(debug = false) ~(config : Config.t) ~(sim : Sim.t) ~montgomery test_inputs =
  let latency = Ec_fpn_mixed_add.latency config in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let internal_ports = Cyclesim.internal_ports sim in
  let dump_stage_if_valid prefix =
    if debug
    then (
      let valid_port_name = prefix ^ "$valid" in
      let valid =
        List.Assoc.find ~equal:String.equal internal_ports valid_port_name
        |> Option.value_exn ~message:(sprintf "Cannot find port %s" valid_port_name)
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
  let test_outputs = ref [] in
  let cycle () =
    Cyclesim.cycle sim;
    for i = 0 to 7 do
      dump_stage_if_valid (sprintf "stage%d" i)
    done;
    if Bits.is_vdd !(outputs.valid_out)
    then (
      let to_z r =
        let r = Bits.to_z ~signedness:Unsigned r in
        if montgomery then transform_from_montgomery r else r
      in
      let z = to_z !(outputs.data_out.z) in
      let x = to_z !(outputs.data_out.x) in
      let y = to_z !(outputs.data_out.y) in
      let x = modulo_multiply x (modulo_inverse Z.(z ** 2)) in
      let y = modulo_multiply y (modulo_inverse Z.(z ** 3)) in
      (* TODO(fyquah): Don't assume the test outputs are always non-infinity. *)
      test_outputs := Ark_bls12_377_g1.create ~x ~y ~infinity:false :: !test_outputs)
  in
  inputs.enable := Bits.vdd;
  List.iter
    test_inputs
    ~f:(fun ({ Jacobian.x = x1; y = y1; z = z1 }, { Affine.x = x0; y = y0 }) ->
      let of_z a =
        Bits.of_z ~width:377 (if montgomery then transform_to_montgomery a else a)
      in
      let rec cycle_until_ready () =
        if Bits.is_gnd !(outputs.ready_in)
        then (
          cycle ();
          cycle_until_ready ())
      in
      cycle_until_ready ();
      inputs.valid_in := Bits.vdd;
      inputs.data_in0.x := of_z x0;
      inputs.data_in0.y := of_z y0;
      inputs.data_in1.x := of_z x1;
      inputs.data_in1.y := of_z y1;
      inputs.data_in1.z := of_z z1;
      inputs.data_in1_z_squared := of_z Z.(z1 * z1 mod p);
      cycle ();
      inputs.valid_in <--. 0;
      inputs.data_in0.x <--. 0;
      inputs.data_in0.y <--. 0;
      inputs.data_in1.z <--. 0;
      inputs.data_in1_z_squared <--. 0;
      inputs.data_in1.x <--. 0;
      inputs.data_in1.y <--. 0);
  inputs.valid_in := Bits.gnd;
  inputs.data_in0.x := Bits.zero 377;
  inputs.data_in0.y := Bits.zero 377;
  inputs.data_in1.z := Bits.zero 377;
  inputs.data_in1_z_squared := Bits.zero 377;
  inputs.data_in1.x := Bits.zero 377;
  inputs.data_in1.y := Bits.zero 377;
  for _ = 0 to latency do
    cycle ()
  done;
  let test_outputs = List.rev !test_outputs in
  let len_test_inputs = List.length test_inputs in
  let len_test_outputs = List.length test_outputs in
  if len_test_inputs <> len_test_outputs
  then
    raise_s
      [%message
        "len(test_inputs) <> len(test_outputs)"
          (len_test_inputs : int)
          (len_test_outputs : int)];
  List.map2_exn test_inputs test_outputs ~f:(fun test_input obtained ->
      let expected =
        let jacobian = fst test_input in
        let affine = snd test_input in
        compute_expected ~jacobian ~affine
      in
      if [%equal: Ark_bls12_377_g1.affine] obtained expected
      then Ok ()
      else
        Or_error.error_s
          [%message
            ""
              ~input:(test_input : Utils.z Jacobian.t * Utils.z Affine.t)
              (obtained : Ark_bls12_377_g1.affine)
              (expected : Ark_bls12_377_g1.affine)])
  |> Or_error.combine_errors_unit
  |> [%sexp_of: unit Or_error.t]
  |> Stdio.print_s
;;
