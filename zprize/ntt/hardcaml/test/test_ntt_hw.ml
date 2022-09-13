open! Base
open Hardcaml
open Hardcaml_waveterm
open Expect_test_helpers_base

let%expect_test "addressing" =
  let module Ntt =
    Ntts_r_fun.Ntt.Make (struct
      let logn = 3
      let twiddle_4step_config = None
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
    │first_4step_pass  ││                                                                    │
    │                  ││────────────────────────────────────────────────────────────────────│
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
    │omegas0           ││ FFFFFFFF00000000                               │0001000000000000   │
    └──────────────────┘└────────────────────────────────────────────────────────────────────┘ |}]
;;

module Gf = Ntts_r_fun.Gf_bits.Make (Bits)

let ( <-- ) a b = a := Bits.of_int ~width:(Bits.width !a) b

let compare_results ~logn ~row ~twiddle_4step_config ~first_4step_pass coefs sim_result =
  let module Ntt = Ntts_r_fun.Ntt_sw.Make (Gf) in
  Ntt.inverse_dit coefs;
  (* if twiddling is enabled (as used for the 4step implementation), model it. *)
  if first_4step_pass
  then (
    match (twiddle_4step_config : Ntts_r_fun.Ntt.twiddle_4step_config option) with
    | None -> ()
    | Some { rows_per_iteration = _; log_num_iterations = _ } ->
      let scl = ref Gf.one in
      let step = ref Gf.one in
      let n2 =
        Ntts_r_fun.Roots.inverse.(logn + logn) |> Ntts_r_fun.Gf_z.to_z |> Gf.of_z
      in
      for _ = 0 to row - 1 do
        step := Gf.mul !step n2
      done;
      for col = 0 to (1 lsl logn) - 1 do
        coefs.(col) <- Gf.mul coefs.(col) !scl;
        scl := Gf.mul !scl !step
      done);
  if not ([%equal: Gf.t array] coefs sim_result)
  then
    print_s
      [%message
        "Simulation results are incorrect."
          (coefs : Gf.Hex.t array)
          (sim_result : Gf.Hex.t array)]
;;

let inverse_ntt_test
  ?twiddle_4step_config
  ?(row = 0)
  ?(first_4step_pass = false)
  ~waves
  input_coefs
  =
  let n = Array.length input_coefs in
  let logn = Int.ceil_log2 n in
  let module Ntt =
    Ntts_r_fun.Ntt.Make (struct
      let logn = logn
      let twiddle_4step_config = twiddle_4step_config
    end)
  in
  let module Sim = Cyclesim.With_interface (Ntt.With_rams.I) (Ntt.With_rams.O) in
  let sim =
    Sim.create
      ~config:Cyclesim.Config.trace_all
      (Ntt.With_rams.create
         ~row
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
  (* twiddle factor debugging*)
  let show_twiddles ~row =
    (*
     {v
        (twiddle_4step_config (((rows_per_iteration 4) (log_num_iterations 1))))
        ((omega (9458d4b40d34071e 6e09f5fb7ea3e241 9c6608d4bcc7fa24 6084e6845a912f4d))
        (scale (9d8f2ad78bfed972 1905d02a5c411f4e 6084e6845a912f4d bf79143ce60ca966))
        (factors (
          (9458d4b40d34071e e096f67d425dc907)
          (6e09f5fb7ea3e241 ef9b216187a69747)
          (9c6608d4bcc7fa24 e9f2da6cbe22ce11)
          (6084e6845a912f4d 54d7ae14ff783309))))
    v}

       {v
        (twiddle_4step_config (((rows_per_iteration 8) (log_num_iterations 1))))
        ((omega (9c6608d4bcc7fa24 3aa7040808edacd5 53a620f7879db5e1 e50ec5b5d3093580))
        (scale (1905d02a5c411f4e bf79143ce60ca966 ba25eb5cd1970aeb f80007ff08000001))
        (factors (
          (9c6608d4bcc7fa24 49a282146535171d)
          (3aa7040808edacd5 341d1690db490d27)
          (53a620f7879db5e1 856cd3bff20269d1)
          (e50ec5b5d3093580 c70224c5386a93ec))))
    v}
         
  *)
    let scale = Ntt.Controller.twiddle_scale_z in
    let omega = Ntt.Controller.twiddle_omega_z row in
    let factors =
      List.init 4 ~f:(fun iter -> Ntt.Controller.twiddle_roots_z ~row ~iter)
    in
    print_s
      [%message
        (omega : Ntts_r_fun.Gf_z.Hex.t list)
          (scale : Ntts_r_fun.Gf_z.Hex.t list)
          (factors : Ntts_r_fun.Gf_z.Hex.t list list)]
  in
  Option.iter twiddle_4step_config ~f:(fun _ -> show_twiddles ~row);
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
  inputs.first_4step_pass := Bits.of_bool first_4step_pass;
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
  compare_results ~logn ~row ~twiddle_4step_config ~first_4step_pass input_coefs result;
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
