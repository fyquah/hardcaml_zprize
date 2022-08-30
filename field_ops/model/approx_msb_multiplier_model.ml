open Base
module Radix = Snarks_r_fun.Radix

let p = Ark_bls12_377_g1.modulus ()
let k = 2 * 377
let m = Z.((one lsl k) / p)
let sel_bottom x bits = Z.(x land ((one lsl bits) - one))

let%expect_test "print constants" =
  Stdio.printf "m = 0x%s\n" (Z.format "x" m);
  [%expect
    {| m = 0x261508d0cc4060e976c3ca0582ef4f73bbad0de6776b1a06af2d488d85a6d02d0ed687789c42a591f9fd58c5e4daffc |}]
;;

let calc_k radix w =
  match radix with
  | Radix.Radix_2 -> Float.to_int (Float.( * ) (Float.of_int w) 0.43)
  | Radix.Radix_3 -> Float.to_int (Float.( * ) (Float.of_int w) 0.33)
;;

let rec estimated_upper_bound_error ~full_multiply_threshold w =
  if w <= full_multiply_threshold
  then Z.zero
  else (
    let k = calc_k Radix_2 w in
    let k2 = k * 2 in
    let child = estimated_upper_bound_error ~full_multiply_threshold (w / 2) in
    Z.((one lsl k2) + ((one lsl k) * (child + child))))
;;

let ceil_div x b =
  let d = Z.(x / b) in
  if Z.(equal (x mod b) zero) then d else Z.(d + one)
;;

let%expect_test "Delta error" =
  Stdio.printf
    "Delta error = 0x%s\n"
    (Z.format
       "x"
       (ceil_div
          (estimated_upper_bound_error ~full_multiply_threshold:23 378)
          Z.(one lsl 377)));
  [%expect {| Delta error = 0x1 |}]
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

let rec approx_msb_multiply ~radices:arg_radices ~w a b =
  match arg_radices with
  | [] ->
    let k = calc_k Radix_2 w in
    let k2 = k * 2 in
    Z.(((a * b) asr k2) lsl k2)
  | hd :: tl ->
    let k = calc_k hd w in
    (match hd with
    | Radix.Radix_2 ->
      let k2 = k * 2 in
      let ua, la = split_top_and_btm ~k a in
      let ub, lb = split_top_and_btm ~k b in
      let ua_mult_lb = Z.(approx_msb_multiply ~radices:tl ~w:Int.(w - k) ua lb lsl k) in
      let ub_mult_la = Z.(approx_msb_multiply ~radices:tl ~w:Int.(w - k) ub la lsl k) in
      let ua_mult_ub = Z.((ua * ub) lsl k2) in
      Z.(ua_mult_ub + ub_mult_la + ua_mult_lb)
    | Radix_3 ->
      let k = calc_k Radix_3 w in
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
      let x2y0 = approx_msb_multiply ~radices:tl ~w:(w - (2 * k)) x2 y0 in
      let x1y1 = approx_msb_multiply ~radices:tl ~w:(w - (2 * k)) x1 y1 in
      let x0y2 = approx_msb_multiply ~radices:tl ~w:(w - (2 * k)) x0 y2 in
      let result =
        Z.((x2y2 lsl k4) + ((x2y1 + x1y2) lsl k3) + ((x2y0 + x1y1 + x0y2) lsl k2))
      in
      assert (Z.equal result Z.zero || Z.log2up result <= 2 * w);
      result)
;;

let%expect_test "" =
  let a = Z.(p - one) in
  let b = m in
  let approx = approx_msb_multiply ~radices:[ Radix_3; Radix_3; Radix_2 ] ~w:378 a b in
  let actual = Z.(a * b) in
  let error = ceil_div Z.(actual - approx) Z.(one lsl 377) in
  Stdio.printf "Error = %s * 2^377\n" (Z.to_string error);
  [%expect {| Error = 1 * 2^377 |}]
;;
