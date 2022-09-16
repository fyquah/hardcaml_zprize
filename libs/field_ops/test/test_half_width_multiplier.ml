open! Base
open! Hardcaml
open! Field_ops_lib
open! Bits

module Half_width_multiplier_377 = Half_width_multiplier.With_interface_multiply (struct
  let bits = 377
end)

module Config = Half_width_multiplier.Config
module I = Half_width_multiplier_377.I
module O = Half_width_multiplier_377.O
module Sim = Cyclesim.With_interface (I) (O)

let create_sim ~config =
  let scope = Scope.create ~flatten_design:true () in
  Sim.create
    ~config:Cyclesim.Config.trace_all
    (Half_width_multiplier_377.create ~config scope)
;;

let rand () = Utils.random_z ~lo_incl:Z.zero ~hi_incl:Z.((of_int 2 ** 377) - one)
let bits_of_z = Bits.of_z ~width:377
let max_value = Z.((one lsl 377) - one)

let test config =
  let sim = create_sim ~config in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  inputs.enable := Bits.vdd;
  let test_cases =
    List.concat
      [ [ Z.zero, Z.zero
        ; max_value, max_value
        ; Z.one, max_value
        ; Z.of_int 2, max_value
        ; Z.of_int 42, Z.of_int 42
        ]
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
  List.iter test_cases ~f:(fun (x, y) ->
      inputs.x := bits_of_z x;
      inputs.y := bits_of_z y;
      cycle ());
  inputs.in_valid := gnd;
  for _ = 1 to Config.latency config do
    cycle ()
  done;
  assert (Queue.length obtained_results = List.length test_cases);
  List.iter2_exn (Queue.to_list obtained_results) test_cases ~f:(fun obtained (x, y) ->
      let mask = Z.((one lsl 377) - one) in
      let expected = Z.(x * y land mask) in
      if not (Z.equal obtained expected)
      then
        raise_s
          [%message
            "Test case failed!" (x : Utils.z) (obtained : Utils.z) (expected : Utils.z)])
;;

let%expect_test "Half-width multiplier with single radix-2 level" =
  test
    { levels =
        [ { pre_adder_stages = 1
          ; middle_adder_stages = 1
          ; post_adder_stages = 1
          ; radix = Radix_2
          }
        ]
    ; ground_multiplier = Verilog_multiply { latency = 1 }
    }
;;

let%expect_test "Half-width multiplier with single radix-3 level" =
  test
    { levels =
        [ { pre_adder_stages = 1
          ; middle_adder_stages = 1
          ; post_adder_stages = 1
          ; radix = Radix_3
          }
        ]
    ; ground_multiplier = Verilog_multiply { latency = 1 }
    }
;;

let%expect_test "Half-width multiplier with mixed radix levels" =
  test
    { levels =
        [ { radix = Radix_2
          ; pre_adder_stages = 1
          ; middle_adder_stages = 1
          ; post_adder_stages = 1
          }
        ; { radix = Radix_2
          ; pre_adder_stages = 1
          ; middle_adder_stages = 1
          ; post_adder_stages = 1
          }
        ; { radix = Radix_2
          ; pre_adder_stages = 1
          ; middle_adder_stages = 1
          ; post_adder_stages = 1
          }
        ]
    ; ground_multiplier = Verilog_multiply { latency = 1 }
    }
;;
