open Core
open Hardcaml

include struct
  open Twisted_edwards_lib
  module Mixed_add = Mixed_add
  module Mixed_add_precompute = Mixed_add_precompute
  module Config = Config
  module Num_bits = Num_bits
  module Xyt = Xyt
  module Xyzt = Xyzt
end

module Model = Twisted_edwards_model_lib
module Utils = Field_ops_test.Utils

module type Test_adder = sig
  module Adder_i : Twisted_edwards_lib.Adder_intf.S

  module Adder : module type of Adder_i.Make (struct
    let num_bits = 377
  end)

  module Xyzt = Adder.Xyzt
  module Xyt = Adder.Xyt
  module Sim : module type of Cyclesim.With_interface (Adder.I) (Adder.O)

  val create_sim : Config.t -> Sim.t

  val test
    :  ?debug:bool
    -> config:Config.t
    -> sim:Sim.t
    -> montgomery:bool
    -> (Z.t Xyzt.t * Z.t Xyt.t * bool) list
    -> unit
end

module Make
  (Adder_i : Twisted_edwards_lib.Adder_intf.S) (R : sig
    val host_precompute : bool
    val arbitrate : bool
  end) : Test_adder = struct
  module Adder_i = Adder_i

  module Adder = Adder_i.Make (struct
    let num_bits = 377
  end)

  module Xyt = Adder.Xyt
  module Xyzt = Adder.Xyzt
  module Sim = Cyclesim.With_interface (Adder.I) (Adder.O)

  let create_sim (config : Config.t) =
    let scope = Scope.create ~flatten_design:true () in
    Sim.create ~config:Cyclesim.Config.trace_all (Adder.create ~config scope)
  ;;

  let p = Ark_bls12_377_g1.modulus ()
  let modulo_inverse x = Utils.modulo_inverse ~p x
  let c_R = Z.(one lsl log2up p)
  let c_R' = modulo_inverse c_R
  let transform_to_montgomery a = Z.(a * c_R mod p)
  let transform_from_montgomery a = Z.(a * c_R' mod p)

  let compute_expected (p1 : Z.t Xyzt.t) (p2 : Z.t Xyt.t) subtract =
    let p3 =
      Model.Twisted_edwards_curve.add_unified
        ~subtract
        (Lazy.force Model.Bls12_377_params.twisted_edwards)
        { x = p1.x; y = p1.y; z = p1.z; t = p1.t }
        { x = p2.x; y = p2.y; t = p2.t }
    in
    { Xyzt.x = p3.x; y = p3.y; z = p3.z; t = p3.t }
  ;;

  let test ?(debug = false) ~(config : Config.t) ~(sim : Sim.t) ~montgomery test_inputs =
    let latency = Adder.latency config in
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
    let input_valid_cycles = ref [] in
    let output_valid_cycles = ref [] in
    let cycle_cnt = ref 0 in
    let cycle () =
      if Bits.is_vdd !(inputs.valid_in)
      then input_valid_cycles := !cycle_cnt :: !input_valid_cycles;
      Cyclesim.cycle sim;
      Int.incr cycle_cnt;
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
        let p3 =
          if R.host_precompute
          then (
            (* convert back out of the internal fpga representation *)
            let twisted_edwards_p3 =
              Model.Twisted_edwards_curve.from_fpga_internal_representation
                { Model.Twisted_edwards_curve.x = p3.x; y = p3.y; z = p3.z; t = p3.t }
            in
            { Xyzt.x = twisted_edwards_p3.x
            ; y = twisted_edwards_p3.y
            ; z = twisted_edwards_p3.z
            ; t = twisted_edwards_p3.t
            })
          else p3
        in
        test_outputs := p3 :: !test_outputs;
        output_valid_cycles := !cycle_cnt :: !output_valid_cycles)
    in
    List.iter test_inputs ~f:(fun ((p1 : Z.t Xyzt.t), (p2 : Z.t Xyt.t), subtract) ->
      let p1, p2 =
        if R.host_precompute
        then (
          (* convert p1 to internal fpga representation and p2 to host precompute representation *)
          let twisted_edwards_p1 =
            Model.Twisted_edwards_curve.to_fpga_internal_representation
              { Model.Twisted_edwards_curve.x = p1.x; y = p1.y; z = p1.z; t = p1.t }
          in
          let twisted_edwards_p2 =
            Model.Twisted_edwards_curve.affine_with_t_to_host_extended_representation
              (Lazy.force Model.Bls12_377_params.twisted_edwards)
              { Model.Twisted_edwards_curve.x = p2.x; y = p2.y; t = p2.t }
          in
          ( { Xyzt.x = twisted_edwards_p1.x
            ; y = twisted_edwards_p1.y
            ; z = twisted_edwards_p1.z
            ; t = twisted_edwards_p1.t
            }
          , { Xyt.x = twisted_edwards_p2.x
            ; y = twisted_edwards_p2.y
            ; t = twisted_edwards_p2.t
            } ))
        else p1, p2
      in
      let of_z a =
        Bits.of_z ~width:377 (if montgomery then transform_to_montgomery a else a)
      in
      let assign_z dst src = dst := of_z src in
      inputs.valid_in := Bits.vdd;
      Xyzt.iter2 ~f:assign_z inputs.p1 p1;
      Xyt.iter2 ~f:assign_z inputs.p2 p2;
      inputs.subtract := Bits.of_bool subtract;
      cycle ();
      if R.arbitrate
      then (
        inputs.valid_in := Bits.gnd;
        cycle ()));
    inputs.valid_in := Bits.gnd;
    for _ = 0 to latency do
      cycle ()
    done;
    let test_outputs = List.rev !test_outputs in
    let output_valid_cycles = List.rev !output_valid_cycles in
    let input_valid_cycles = List.rev !input_valid_cycles in
    let len_test_inputs = List.length test_inputs in
    let len_test_outputs = List.length test_outputs in
    if len_test_inputs <> len_test_outputs
    then
      raise_s
        [%message
          "len(test_inputs) <> len(test_outputs)"
            (len_test_inputs : int)
            (len_test_outputs : int)];
    List.iter2_exn
      input_valid_cycles
      output_valid_cycles
      ~f:(fun input_valid_cycle output_valid_cycle ->
      assert (output_valid_cycle - input_valid_cycle = latency));
    List.map2_exn test_inputs test_outputs ~f:(fun test_input obtained ->
      let expected =
        let a, b, subtract = test_input in
        compute_expected a b subtract
      in
      if [%equal: Z.t Xyzt.t] obtained expected
      then Ok ()
      else
        Or_error.error_s
          [%message
            ""
              ~input:(test_input : Utils.z Xyzt.t * Utils.z Xyt.t * bool)
              (obtained : Utils.z Xyzt.t)
              (expected : Utils.z Xyzt.t)])
    |> Or_error.combine_errors_unit
    |> [%sexp_of: unit Or_error.t]
    |> Stdio.print_s
  ;;
end

module Test_mixed_add =
  Make
    (Twisted_edwards_lib.Mixed_add)
    (struct
      let host_precompute = false
      let arbitrate = true
    end)

module Test_mixed_add_precompute =
  Make
    (Twisted_edwards_lib.Mixed_add_precompute)
    (struct
      let host_precompute = true
      let arbitrate = true
    end)

module Test_mixed_add_precompute_full =
  Make
    (Twisted_edwards_lib.Mixed_add_precompute)
    (struct
      let host_precompute = true
      let arbitrate = false
    end)
