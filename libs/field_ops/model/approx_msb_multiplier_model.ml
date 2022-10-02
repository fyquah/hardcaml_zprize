open Base
open Core

let p = Ark_bls12_377_g1.modulus ()
let k = 2 * 377
let m = Z.((one lsl k) / p)
let sel_bottom x bits = Z.(x land ((one lsl bits) - one))

let%expect_test "print constants" =
  Stdio.printf "m = 0x%s\n" (Z.format "x" m);
  [%expect
    {| m = 0x261508d0cc4060e976c3ca0582ef4f73bbad0de6776b1a06af2d488d85a6d02d0ed687789c42a591f9fd58c5e4daffc |}]
;;

module Level = struct
  type t =
    { radix : Radix.t
    ; k : int -> int
    }
end

let rec estimated_upper_bound_error ~levels ~lo ~w =
  assert (w > 0);
  match levels with
  | [] ->
    Stdio.printf "Ground multiplier width = %d\n" w;
    Z.zero
  | (hd : Level.t) :: tl ->
    let k = hd.k w in
    Stdio.printf "w=%d lo=%d k=%d\n" w lo k;
    let k2 = k * 2 in
    (match hd.radix with
     | Radix_2 ->
       (* x1y1 * 2^2k
        * + (x1y0 + x0y1) * 2^k  <-- partial multiplication here
        * + x0y0
        *)
       let child = estimated_upper_bound_error ~levels:tl ~lo:(lo + k) ~w:(w - k) in
       Z.((one lsl k2) + ((one lsl k) * (child + child)))
     | Radix_3 ->
       (*
        *  = x2y2 * 2^4k
        *    + (x2y1 + x1y2) * 2^3k
        *    + (x2y0 + x1y1 + x0y2) * 2^2k
        *    + (x1y0 + x0y1) * 2^k
        *    + x0y0
        *
        * Approximated by
        *
        *  = x2y2 * 2^4k
        *    + (x2y1 + x1y2) * 2^3k
        *    + (x2y0 + x1y1 + x0y2) * 2^2k  <-- use approximation recursively here.
        *
        *  The error terms are:
        *  -    1 * 2^(2k)                        (due to x0y0)
        *  -  2^k * (2^(2k) + 2^(2k))             (due to (x1y0 + x0y1) * 2^k)
        *  - 2^2k * (recur() + recur() + recur())
        *)
       let t0 = Z.((one lsl k2) lsl 0) in
       let t1 = Z.(((one lsl k2) + (one lsl k2)) lsl k) in
       let t2 =
         let child = estimated_upper_bound_error ~levels:tl ~lo:(lo + k2) ~w:(w - k2) in
         Z.((child + child + child) lsl k2)
       in
       Z.(t0 + t1 + t2))
;;

let ceil_div x b =
  let d = Z.(x / b) in
  if Z.(equal (x mod b) zero) then d else Z.(d + one)
;;

let estimate_delta_error_2_to_n levels =
  ceil_div (estimated_upper_bound_error ~levels ~w:378 ~lo:0) Z.(one lsl 377)
;;

let golden_config_2222 =
  let open Level in
  [ { radix = Radix_2; k = (fun _ -> 187) }
  ; { radix = Radix_2; k = (fun _ -> 94) }
  ; { radix = Radix_2; k = (fun _ -> 48) }
  ; { radix = Radix_2; k = (fun _ -> 24) }
  ]
;;

let golden_config_332 =
  let open Level in
  [ { radix = Radix_3; k = (fun _ -> 125) }
  ; { radix = Radix_3; k = (fun _ -> 42) }
  ; { radix = Radix_2; k = (fun _ -> 20) }
  ]
;;

let%expect_test "Delta error on 332" =
  Stdio.printf
    "Delta error = %s * (2^377)\n"
    (Z.to_string (estimate_delta_error_2_to_n golden_config_332));
  [%expect
    {|
    w=378 lo=0 k=125
    w=128 lo=250 k=42
    w=44 lo=334 k=20
    Ground multiplier width = 24
    Delta error = 5 * (2^377) |}]
;;

let%expect_test "Delta error on 2222" =
  Stdio.printf
    "Delta error = %s * (2^377)\n"
    (Z.to_string (estimate_delta_error_2_to_n golden_config_2222));
  [%expect
    {|
    w=378 lo=0 k=187
    w=191 lo=187 k=94
    w=97 lo=281 k=48
    w=49 lo=329 k=24
    Ground multiplier width = 25
    Delta error = 13 * (2^377) |}]
;;

let build_precompute_two num_bits =
  let ret = Int.Table.create () in
  Hashtbl.set ret ~key:0 ~data:Z.zero;
  let rec loop idx =
    if idx = 1 lsl num_bits
    then ()
    else (
      (* our number has a prefix of [idx], so we want to subtract off the largest multiple of
       * [p] with a prefix of [idx-1] *)
      let prefix = Z.of_int (idx - 1) in
      let max_with_prefix = Z.((prefix lsl 377) + ((one lsl 377) - one)) in
      let mult = Z.(max_with_prefix / p) in
      let mult_times_p = Z.(mult * p) in
      let () =
        let tmp = Z.((mult + one) * p) in
        assert (Z.(geq tmp ((prefix + one) lsl 377)));
        assert (Z.(equal (mult_times_p asr 377) prefix))
      in
      Hashtbl.set ret ~key:idx ~data:mult_times_p;
      loop (idx + 1))
  in
  loop 1;
  ret
;;

let%expect_test "precompute table" =
  let lookup = build_precompute_two 5 |> Hashtbl.map ~f:(fun v -> Z.(v / p |> to_int)) in
  print_s [%message (lookup : int Int.Table.t)];
  [%expect
    {|
    (lookup
     ((0 0) (1 1) (2 2) (3 3) (4 4) (5 5) (6 7) (7 8) (8 9) (9 10) (10 11)
      (11 13) (12 14) (13 15) (14 16) (15 17) (16 19) (17 20) (18 21) (19 22)
      (20 23) (21 24) (22 26) (23 27) (24 28) (25 29) (26 30) (27 32) (28 33)
      (29 34) (30 35) (31 36))) |}]
;;

let build_precompute ?(verbose = false) delta_error =
  let open Hardcaml in
  let top_bits = Int.ceil_log2 delta_error in
  let lookup = Int.Table.create () in
  for i = 0 to (1 lsl top_bits) - 1 do
    (* CR rahuly: if we really need (1 lsl top_bits), we can just mux the input to the bram and set it to 0 *)
    (* start from the top, and add the highest multiple of p that we can subtract without going
     * negative *)
    let i = (1 lsl top_bits) - i in
    let ip = Z.(p * of_int i) in
    let b = Bits.of_z ip ~width:(377 + top_bits) in
    let top = Bits.sel_top b top_bits in
    if verbose
    then print_s [%message (i : int) (top : Bits.t) (i - Bits.to_int top : int)];
    let _r = Hashtbl.add lookup ~key:(Bits.to_int top + 1) ~data:i in
    ()
  done;
  lookup
;;

let%expect_test "build a precomputed correction table" =
  let lookup = build_precompute ~verbose:true 32 in
  print_s [%message (lookup : int Int.Table.t)];
  [%expect
    {|
    ((i 32) (top 11010) ("i - (Bits.to_int top)" 6))
    ((i 31) (top 11010) ("i - (Bits.to_int top)" 5))
    ((i 30) (top 11001) ("i - (Bits.to_int top)" 5))
    ((i 29) (top 11000) ("i - (Bits.to_int top)" 5))
    ((i 28) (top 10111) ("i - (Bits.to_int top)" 5))
    ((i 27) (top 10110) ("i - (Bits.to_int top)" 5))
    ((i 26) (top 10101) ("i - (Bits.to_int top)" 5))
    ((i 25) (top 10101) ("i - (Bits.to_int top)" 4))
    ((i 24) (top 10100) ("i - (Bits.to_int top)" 4))
    ((i 23) (top 10011) ("i - (Bits.to_int top)" 4))
    ((i 22) (top 10010) ("i - (Bits.to_int top)" 4))
    ((i 21) (top 10001) ("i - (Bits.to_int top)" 4))
    ((i 20) (top 10000) ("i - (Bits.to_int top)" 4))
    ((i 19) (top 01111) ("i - (Bits.to_int top)" 4))
    ((i 18) (top 01111) ("i - (Bits.to_int top)" 3))
    ((i 17) (top 01110) ("i - (Bits.to_int top)" 3))
    ((i 16) (top 01101) ("i - (Bits.to_int top)" 3))
    ((i 15) (top 01100) ("i - (Bits.to_int top)" 3))
    ((i 14) (top 01011) ("i - (Bits.to_int top)" 3))
    ((i 13) (top 01010) ("i - (Bits.to_int top)" 3))
    ((i 12) (top 01010) ("i - (Bits.to_int top)" 2))
    ((i 11) (top 01001) ("i - (Bits.to_int top)" 2))
    ((i 10) (top 01000) ("i - (Bits.to_int top)" 2))
    ((i 9) (top 00111) ("i - (Bits.to_int top)" 2))
    ((i 8) (top 00110) ("i - (Bits.to_int top)" 2))
    ((i 7) (top 00101) ("i - (Bits.to_int top)" 2))
    ((i 6) (top 00101) ("i - (Bits.to_int top)" 1))
    ((i 5) (top 00100) ("i - (Bits.to_int top)" 1))
    ((i 4) (top 00011) ("i - (Bits.to_int top)" 1))
    ((i 3) (top 00010) ("i - (Bits.to_int top)" 1))
    ((i 2) (top 00001) ("i - (Bits.to_int top)" 1))
    ((i 1) (top 00000) ("i - (Bits.to_int top)" 1))
    (lookup
     ((1 1) (2 2) (3 3) (4 4) (5 5) (6 7) (7 8) (8 9) (9 10) (10 11) (11 13)
      (12 14) (13 15) (14 16) (15 17) (16 19) (17 20) (18 21) (19 22) (20 23)
      (21 24) (22 26) (23 27) (24 28) (25 29) (26 30) (27 32))) |}]
;;

let split_top_and_btm ~k a =
  let top = Z.(a asr k) in
  let btm = Z.(a land ((one lsl k) - one)) in
  top, btm
;;

let split3 ~k a =
  let k2 = k * 2 in
  let top = Z.(a asr k2) in
  let mid = sel_bottom Z.(a asr k) k in
  let btm = sel_bottom a k in
  top, mid, btm
;;

let rec approx_msb_multiply ~(levels : Level.t list) ~w a b =
  match levels with
  | [] -> Z.(a * b)
  | hd :: tl ->
    let k = hd.k w in
    (match hd.radix with
     | Radix.Radix_2 ->
       let k2 = k * 2 in
       let ua, la = split_top_and_btm ~k a in
       let ub, lb = split_top_and_btm ~k b in
       let ua_mult_lb = Z.(approx_msb_multiply ~levels:tl ~w:Int.(w - k) ua lb lsl k) in
       let ub_mult_la = Z.(approx_msb_multiply ~levels:tl ~w:Int.(w - k) ub la lsl k) in
       let ua_mult_ub = Z.((ua * ub) lsl k2) in
       Z.(ua_mult_ub + ub_mult_la + ua_mult_lb)
     | Radix_3 ->
       let k2 = k * 2 in
       let k3 = k * 3 in
       let k4 = k * 4 in
       let x2, x1, x0 = split3 ~k a in
       let y2, y1, y0 = split3 ~k b in
       (*
        *  = x2y2 * 2^4k
        *    + (x2y1 + x1y2) * 2^3k
        *    + (x2y0 + x1y1 + x0y2) * 2^2k
        *    + (x1y0 + x0y1) * 2^k
        *    + x0y0
        *
        * Approximated by
        *
        *  = x2y2 * 2^4k
        *    + (x2y1 + x1y2) * 2^3k
        *    + (x2y0 + x1y1 + x0y2) * 2^2k  <-- use approximation recursively here.
        *)
       let x2y2 = Z.(x2 * y2) in
       let x2y1 = Z.(x2 * y1) in
       let x1y2 = Z.(x1 * y2) in
       let x2y0 = approx_msb_multiply ~levels:tl ~w:(w - (2 * k)) x2 y0 in
       let x1y1 = approx_msb_multiply ~levels:tl ~w:(w - (2 * k)) x1 y1 in
       let x0y2 = approx_msb_multiply ~levels:tl ~w:(w - (2 * k)) x0 y2 in
       let result =
         Z.((x2y2 lsl k4) + ((x2y1 + x1y2) lsl k3) + ((x2y0 + x1y1 + x0y2) lsl k2))
       in
       assert (Z.equal result Z.zero || Z.log2up result <= 2 * w);
       result)
;;

let%expect_test "" =
  let a = Z.(p - one) in
  let b = m in
  let approx = approx_msb_multiply ~levels:golden_config_332 ~w:378 a b in
  let actual = Z.(a * b) in
  let error = ceil_div Z.(actual - approx) Z.(one lsl 377) in
  Stdio.printf "Error = %s * 2^377\n" (Z.to_string error);
  [%expect {| Error = 1 * 2^377 |}]
;;
