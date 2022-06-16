open! Base
open! Hardcaml
open! Snarks_r_fun
open! Bits

module Half_width_multiplier377 = Half_width_multiplier.With_interface_multiply (struct
  let bits = 377
end)

module Config = Half_width_multiplier.Config
module I = Half_width_multiplier377.I
module O = Half_width_multiplier377.O
module Sim = Cyclesim.With_interface (I) (O)

let create_sim ~config =
  let scope = Scope.create ~flatten_design:true () in
  Sim.create
    ~config:Cyclesim.Config.trace_all
    (Half_width_multiplier377.create ~config scope)
;;

let rand () = Utils.random_z ~lo_incl:Z.zero ~hi_incl:Z.((of_int 2 ** 377) - one)
let bits_of_z = Bits.of_z ~width:377
let max_value = Z.((one lsl 377) - one)

let%expect_test "" =
  let config =
    { Config.depth = 4; ground_multiplier = Verilog_multiply { latency = 1 } }
  in
  let sim = create_sim ~config in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  inputs.enable := Bits.vdd;
  let test_cases =
    List.concat
      [ [ Z.zero, max_value; max_value, max_value; max_value, Z.one ]
      ; List.init 50 ~f:(fun _ -> rand (), rand ())
      ]
  in
  let obtained_results = Queue.create () in
  let cycle () =
    Cyclesim.cycle sim;
    if is_vdd !(outputs.out_valid)
    then Queue.enqueue obtained_results (Bits.to_z ~signedness:Unsigned !(outputs.z))
  in
  inputs.in_valid := vdd;
  List.iter test_cases ~f:(fun (a, b) ->
      inputs.x := bits_of_z a;
      inputs.y := bits_of_z b;
      cycle ());
  inputs.in_valid := gnd;
  for _ = 1 to Config.latency config do
    cycle ()
  done;
  assert (Queue.length obtained_results = List.length test_cases);
  List.iter2_exn (Queue.to_list obtained_results) test_cases ~f:(fun obtained (x, y) ->
      let expected = Z.(x * y land max_value) in
      if not (Z.equal obtained expected)
      then
        raise_s
          [%message
            "Test case failed!"
              (x : Utils.z)
              (y : Utils.z)
              (obtained : Utils.z)
              (expected : Utils.z)])
;;
