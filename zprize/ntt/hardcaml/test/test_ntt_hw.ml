open! Base
open Hardcaml
open Hardcaml_waveterm
open Expect_test_helpers_base

let%expect_test "addressing" =
  let module Ntt =
    Ntts_r_fun.Ntt.Make (struct
      let logn = 3
    end)
  in
  let module Sim = Cyclesim.With_interface (Ntt.Controller.I) (Ntt.Controller.O) in
  let sim = Sim.create (Ntt.Controller.create (Scope.create ~flatten_design:true ())) in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  inputs.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clear := Bits.gnd;
  inputs.start := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.start := Bits.gnd;
  for _ = 0 to 14 do
    Cyclesim.cycle sim
  done;
  Waveform.print ~display_height:38 ~wave_width:1 ~display_width:90 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
    │clock             ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ │
    │                  ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─│
    │clear             ││────┐                                                               │
    │                  ││    └───────────────────────────────────────────────────────────────│
    │start             ││    ┌───┐                                                           │
    │                  ││────┘   └───────────────────────────────────────────────────────────│
    │                  ││────────────┬───┬───┬───┬───────────────────────┬───┬───┬───┬───┬───│
    │addr1             ││ 0          │2  │4  │6  │7                      │0  │1  │4  │5  │6  │
    │                  ││────────────┴───┴───┴───┴───────────────────────┴───┴───┴───┴───┴───│
    │                  ││────────┬───┬───┬───┬───┬───────────────────────┬───┬───┬───┬───┬───│
    │addr2             ││ 0      │1  │3  │5  │7  │0                      │2  │3  │6  │7  │0  │
    │                  ││────────┴───┴───┴───┴───┴───────────────────────┴───┴───┴───┴───┴───│
    │done_             ││    ┌───┐                                                           │
    │                  ││────┘   └───────────────────────────────────────────────────────────│
    │first_stage       ││        ┌───────────────────────────────────────┐                   │
    │                  ││────────┘                                       └───────────────────│
    │flip              ││                                            ┌───┐                   │
    │                  ││────────────────────────────────────────────┘   └───────────────────│
    │                  ││────────────────────────────────────────────────┬───────────────────│
    │i                 ││ 0                                              │1                  │
    │                  ││────────────────────────────────────────────────┴───────────────────│
    │                  ││────────────────────────┬───────────────────────┬───┬───┬───┬───┬───│
    │j                 ││ 0                      │1                      │0  │1  │0  │1  │2  │
    │                  ││────────────────────────┴───────────────────────┴───┴───┴───┴───┴───│
    │                  ││────────────┬───┬───┬───────────────────────────┬───────┬───────────│
    │k                 ││ 0          │2  │4  │6                          │0      │4          │
    │                  ││────────────┴───┴───┴───────────────────────────┴───────┴───────────│
    │last_stage        ││                                                                    │
    │                  ││────────────────────────────────────────────────────────────────────│
    │                  ││────────┬───────────────────────────────────────┬───────────────────│
    │m                 ││ 0      │1                                      │2                  │
    │                  ││────────┴───────────────────────────────────────┴───────────────────│
    │                  ││────────────────────────────────────────────────┬───────────────────│
    │omega1            ││ FFFFFFFF00000000                               │0001000000000000   │
    │                  ││────────────────────────────────────────────────┴───────────────────│
    │                  ││────────────────────────────────────────────────┬───────────────────│
    └──────────────────┘└────────────────────────────────────────────────────────────────────┘ |}]
;;

module Gf = Ntts_r_fun.Gf_bits.Make (Bits)

let ( <-- ) a b = a := Bits.of_int ~width:(Bits.width !a) b

let compare_results coefs sim_result =
  let module Ntt = Ntts_r_fun.Ntt_sw.Make (Gf) in
  Ntt.inverse_dit coefs;
  if not ([%equal: Gf.t array] coefs sim_result)
  then
    print_s
      [%message
        "Simulation results are incorrect." (coefs : Gf.t array) (sim_result : Gf.t array)]
;;

let inverse_ntt_test ~waves input_coefs =
  let n = Array.length input_coefs in
  let logn = Int.ceil_log2 n in
  let module Ntt =
    Ntts_r_fun.Ntt.Make (struct
      let logn = logn
    end)
  in
  let module Sim = Cyclesim.With_interface (Ntt.With_rams.I) (Ntt.With_rams.O) in
  let sim =
    Sim.create
      ~config:Cyclesim.Config.trace_all
      (Ntt.With_rams.create
         ~build_mode:Simulation
         (Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ()))
  in
  let waves, sim =
    if waves
    then (
      let waves, sim = Waveform.create sim in
      Some waves, sim)
    else None, sim
  in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  inputs.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clear := Bits.gnd;
  Cyclesim.cycle sim;
  (* load the ram *)
  inputs.wr_en <-- 1;
  Array.iteri input_coefs ~f:(fun addr coef ->
      inputs.wr_addr <-- addr;
      inputs.wr_d := Gf.to_bits coef;
      Cyclesim.cycle sim);
  inputs.wr_en <-- 0;
  (* flip rams *)
  inputs.flip <-- 1;
  Cyclesim.cycle sim;
  inputs.flip <-- 0;
  Cyclesim.cycle sim;
  (* start the core *)
  inputs.start <-- 1;
  Cyclesim.cycle sim;
  inputs.start <-- 0;
  (* poll for done *)
  while not (Bits.to_bool !(outputs.done_)) do
    Cyclesim.cycle sim
  done;
  (* flush *)
  for _ = 0 to 1 do
    Cyclesim.cycle sim
  done;
  (* flip rams *)
  inputs.flip <-- 1;
  Cyclesim.cycle sim;
  inputs.flip <-- 0;
  Cyclesim.cycle sim;
  (* Read results *)
  let result = Array.create ~len:n Gf.zero in
  inputs.rd_en <-- 1;
  inputs.rd_addr <-- 0;
  Cyclesim.cycle sim;
  for i = 1 to n do
    inputs.rd_addr <-- i;
    result.(i - 1) <- Gf.of_bits !(outputs.rd_q);
    Cyclesim.cycle sim
  done;
  inputs.rd_en <-- 0;
  for _ = 0 to 11 do
    Cyclesim.cycle sim
  done;
  compare_results input_coefs result;
  waves, result
;;

let print_waves =
  Waveform.print
    ~display_height:150
    ~wave_width:1
    ~display_width:160
    ~display_rules:
      [ Display_rule.port_name_matches
          Re.Posix.(compile (re ".*"))
          ~wave_format:(Bit_or Unsigned_int)
      ]
;;

let%expect_test "8pt linear" =
  let waves, result =
    inverse_ntt_test
      ~waves:false
      (Array.init 8 ~f:(function
          | 0 -> Gf.one
          | 1 -> Gf.two
          | _ -> Gf.zero))
  in
  let result =
    Array.map result ~f:(fun b ->
        Gf.to_bits b |> Bits.to_constant |> Constant.to_hex_string ~signedness:Unsigned)
  in
  print_s [%message (result : string array)];
  Option.iter waves ~f:print_waves;
  [%expect
    {|
    (result (
      0000000000000003
      fffffffefe000002
      0002000000000001
      fffffdff00000202
      ffffffff00000000
      0000000002000001
      fffdffff00000002
      000001fffffffe01)) |}]
;;

let%expect_test "8pt random" =
  let waves, result =
    inverse_ntt_test
      ~waves:false
      ([| "0xcef967e3e1d0860e"
        ; "0x44be7570bcd4f9df"
        ; "0xf4848ed283e858f2"
        ; "0xa3a3a47eeb6f76f6"
        ; "0xa12d1d0b69c4108b"
        ; "0xeb285d19459ef6c3"
        ; "0x10d812558ad9c103"
        ; "0xd19d3e319d1b6b4a"
       |]
      |> Array.map ~f:(fun z -> Z.of_string z |> Gf.of_z))
  in
  let result =
    Array.map result ~f:(fun b ->
        Gf.to_bits b |> Bits.to_constant |> Constant.to_hex_string ~signedness:Unsigned)
  in
  print_s [%message (result : string array)];
  Option.iter waves ~f:print_waves;
  [%expect
    {|
    (result (
      1aaadb56e555836b
      975bcb9d395a282f
      69055db04cf94815
      963cdab11477cc1c
      d05b70dbcf57ddad
      ed14bc2fbdc30962
      6c8e69de2cabb133
      9c83c8e1d49cd861)) |}]
;;
