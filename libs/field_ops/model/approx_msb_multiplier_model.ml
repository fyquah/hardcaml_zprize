open Base
module Radix = Field_ops_lib.Radix

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

let calc_k radix w =
  match radix with
  | Radix.Radix_2 -> Float.to_int (Float.( * ) (Float.of_int w) 0.43)
  | Radix.Radix_3 -> Float.to_int (Float.( * ) (Float.of_int w) 0.33)
;;

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

let golden_config =
  let open Level in
  [ { radix = Radix_2; k = (fun _ -> 186) }
  ; { radix = Radix_2; k = (fun _ -> 94) }
  ; { radix = Radix_2; k = (fun _ -> 48) }
  ; { radix = Radix_2; k = (fun _ -> 24) }
  ]
;;

let%expect_test "Delta error" =
  Stdio.printf
    "Delta error = %s * (2^377)\n"
    (Z.to_string (estimate_delta_error_2_to_n golden_config));
  [%expect
    {|
    w=378 lo=0 k=186
    w=192 lo=186 k=94
    w=98 lo=280 k=48
    w=50 lo=328 k=24
    Ground multiplier width = 26
    Delta error = 7 * (2^377) |}]
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
  let approx = approx_msb_multiply ~levels:golden_config ~w:378 a b in
  let actual = Z.(a * b) in
  let error = ceil_div Z.(actual - approx) Z.(one lsl 377) in
  Stdio.printf "Error = %s * 2^377\n" (Z.to_string error);
  [%expect {| Error = 2 * 2^377 |}]
;;
