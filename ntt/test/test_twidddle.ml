open Base
open Hardcaml
open Hardcaml_waveterm
open Expect_test_helpers_base
module Gf = Ntts_r_fun.Gf_z
module Ntt = Ntts_r_fun.Ntt_sw.Make (Gf)
module Ntt_hw = Ntts_r_fun.Ntt_4step

let%expect_test "show twiddle generation" =
  let n = 8 in
  let a = Array.init n ~f:(fun _ -> Array.init n ~f:(fun _ -> Gf.one)) in
  Ntt.apply_twiddles Ntt.inverse_roots.(n) a;
  print_s [%message (a : Gf.t array array)];
  (* note; symmetric across the diagonal

     Implemented iteratively, we calculate Ti = T(i-1) * w. The multiplier will
     have a given pipeline latency (maybe 4 clocks), so this feedback is a
     problem.

     Can we do it in batches?

     T0 = 1
     T1 = T0 * w
     T2 = T0 * w2
     T3 = T0 * w3
     ....but then what?????


     0 0 0 0
     0 1 2 3
     0 2 4 6
     0 3 6 9

  *)
  [%expect
    {|
    (a (
      (1 1 1 1 1 1 1 1)
      (1
       13797081185216407910
       17870292113338400769
       281721071064741919
       549755813888
       14041890976876060974
       1125917086449664
       4299803665592489687)
      (1
       17870292113338400769
       549755813888
       1125917086449664
       70368744161280
       2198989700608
       18446744069412487169
       18446744065119551490)
      (1
       281721071064741919
       1125917086449664
       3051558327610197629
       18446744069412487169
       411429644661718300
       18410715272395620481
       18142929134658341675)
      (1
       549755813888
       70368744161280
       18446744069412487169
       17293822564807737345
       8
       4398046511104
       562949953290240)
      (1
       14041890976876060974
       2198989700608
       411429644661718300
       8
       1654663398520981866
       17591917604864
       3291437157293746400)
      (1
       1125917086449664
       18446744069412487169
       18410715272395620481
       4398046511104
       17591917604864
       9223372032559808513
       562949953421314)
      (1
       4299803665592489687
       18446744065119551490
       18142929134658341675
       562949953290240
       3291437157293746400
       562949953421314
       10832292272906805046))) |}];
  (* Iteratively generate the triddles in batches. This is needed to hide the
     latency of the multiplier.Arith_status

     So far we can hide it within rows, though I haven't yet worked out how to
     hide it as we go to the next set of rows (although I think I see how it's
     done, and it will require a few extra cycles to calculate).
  *)
  let root = Ntt.inverse_roots.(n) in
  let r4 = Gf.pow root 4 in
  let initial_roots row = Array.init 4 ~f:(fun col -> Gf.pow root (row * col)) in
  let twiddle_row row =
    let initial = initial_roots row in
    let step = Gf.pow root (row * 4) in
    let next = Array.map initial ~f:(Gf.( * ) step) in
    print_s [%message (initial : Gf.t array) (next : Gf.t array)]
  in
  print_s [%message (root : Gf.t) (r4 : Gf.t)];
  twiddle_row 0;
  twiddle_row 1;
  twiddle_row 4;
  twiddle_row 5;
  [%expect
    {|
    ((root 13797081185216407910)
     (r4   549755813888))
    ((initial (1 1 1 1))
     (next    (1 1 1 1)))
    ((initial (1 13797081185216407910 17870292113338400769 281721071064741919))
     (next (
       549755813888 14041890976876060974 1125917086449664 4299803665592489687)))
    ((initial (1 549755813888 70368744161280 18446744069412487169))
     (next (17293822564807737345 8 4398046511104 562949953290240)))
    ((initial (1 14041890976876060974 2198989700608 411429644661718300))
     (next (8 1654663398520981866 17591917604864 3291437157293746400))) |}];
  let i0 = initial_roots 0 in
  let i1 = initial_roots 1 in
  let i4 = Array.map i0 ~f:(Gf.( * ) r4) in
  let i5 = Array.map i1 ~f:(Gf.( * ) r4) in
  print_s [%message (i4 : Gf.t array) (i5 : Gf.t array)];
  [%expect
    {|
    ((i4 (549755813888 549755813888 549755813888 549755813888))
     (i5 (549755813888 14041890976876060974 1125917086449664 4299803665592489687))) |}]
;;

let%expect_test "twiddle controller" =
  let module Twiddle = Ntt_hw.Twiddle_controller in
  let module Sim = Cyclesim.With_interface (Twiddle.I) (Twiddle.O) in
  let sim = Sim.create (Twiddle.create (Scope.create ())) in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  inputs.start := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.start := Bits.gnd;
  for _ = 0 to 16 do
    Cyclesim.cycle sim
  done;
  Waveform.print ~display_width:94 ~wave_width:1 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────────┐
    │clock             ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ │
    │                  ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─│
    │clear             ││                                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │start             ││────┐                                                                   │
    │                  ││    └───────────────────────────────────────────────────────────────────│
    │                  ││────────┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───│
    │addr              ││ 00     │01 │02 │03 │04 │05 │06 │07 │08 │09 │0A │0B │0C │0D │0E │0F │10 │
    │                  ││────────┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───│
    │done_             ││────┐                                                                   │
    │                  ││    └───────────────────────────────────────────────────────────────────│
    │                  ││                                                                        │
    │                  ││                                                                        │
    │                  ││                                                                        │
    │                  ││                                                                        │
    │                  ││                                                                        │
    │                  ││                                                                        │
    │                  ││                                                                        │
    └──────────────────┘└────────────────────────────────────────────────────────────────────────┘ |}]
;;
