open Core
open Hardcaml

include struct
  open Twisted_edwards_lib
  module Mixed_add = Mixed_add
  module Num_bits = Num_bits
end

module Model = Twisted_edwards_model_lib
module Utils = Field_ops_test.Utils
module Config = Mixed_add.Config

module Mixed_add = Mixed_add.Make (struct
  let num_bits = 377
end)

module Sim = Cyclesim.With_interface (Mixed_add.I) (Mixed_add.O)
module Xyt = Mixed_add.Xyt
module Xyzt = Mixed_add.Xyzt

let create_sim (config : Config.t) =
  let scope = Scope.create ~flatten_design:true () in
  Sim.create ~config:Cyclesim.Config.trace_all (Mixed_add.create ~config scope)
;;

let p = Ark_bls12_377_g1.modulus ()
let modulo_inverse x = Utils.modulo_inverse ~p x
let c_R = Z.(one lsl log2up p)
let c_R' = modulo_inverse c_R
let transform_to_montgomery a = Z.(a * c_R mod p)
let transform_from_montgomery a = Z.(a * c_R' mod p)

let compute_expected (p1 : Z.t Mixed_add.Xyzt.t) (p2 : Z.t Mixed_add.Xyt.t) =
  let p3 =
    Model.Twisted_edwards_curve.add_unified
      (Lazy.force Model.Bls12_377_params.twisted_edwards)
      { x = p1.x; y = p1.y; z = p1.z; t = p1.t }
      { x = p2.x; y = p2.y; t = p2.t }
  in
  { Mixed_add.Xyzt.x = p3.x; y = p3.y; z = p3.z; t = p3.t }
;;

let test ?(debug = false) ~(config : Config.t) ~(sim : Sim.t) ~montgomery test_inputs =
  let latency = Mixed_add.latency config in
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
    for i = 1 to 4 do
      dump_stage_if_valid (sprintf "stage%d" i)
    done;
    if Bits.is_vdd !(outputs.valid_out)
    then (
      let to_z r =
        let r = Bits.to_z ~signedness:Unsigned r in
        if montgomery then transform_from_montgomery r else r
      in
      let p3 = Xyzt.map ~f:(fun x -> to_z !x) outputs.p3 in
      test_outputs := p3 :: !test_outputs)
  in
  List.iter test_inputs ~f:(fun (p1, p2) ->
      let of_z a =
        Bits.of_z ~width:377 (if montgomery then transform_to_montgomery a else a)
      in
      let assign_z dst src = dst := of_z src in
      inputs.valid_in := Bits.vdd;
      Xyzt.iter2 ~f:assign_z inputs.p1 p1;
      Xyt.iter2 ~f:assign_z inputs.p2 p2;
      cycle ();
      inputs.valid_in := Bits.gnd;
      cycle ());
  inputs.valid_in := Bits.gnd;
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
        let a, b = test_input in
        compute_expected a b
      in
      if [%equal: Z.t Xyzt.t] obtained expected
      then Ok ()
      else
        Or_error.error_s
          [%message
            ""
              ~input:(test_input : Utils.z Xyzt.t * Utils.z Xyt.t)
              (obtained : Utils.z Xyzt.t)
              (expected : Utils.z Xyzt.t)])
  |> Or_error.combine_errors_unit
  |> [%sexp_of: unit Or_error.t]
  |> Stdio.print_s
;;
