open Base
open Hardcaml
open Signal

module Config = struct
  type t =
    | Verilog_multiply of { latency : int }
    | Hybrid_dsp_and_luts of { latency : int }
    | Specialized_45_bit_multiply
  [@@deriving sexp_of]

  let latency = function
    | Verilog_multiply { latency } -> latency
    | Hybrid_dsp_and_luts { latency } -> latency
    | Specialized_45_bit_multiply -> 5
  ;;
end

let long_multiplication_with_addition
    (type a)
    (module Comb : Comb.S with type t = a)
    ~pivot
    big
  =
  let open Comb in
  let output_width = width pivot + width big in
  let addition_terms =
    List.filter_mapi (bits_lsb pivot) ~f:(fun i b ->
        let term = mux2 b (concat_msb_e [ big; zero i ]) (zero (i + width big)) in
        if is_vdd (term ==:. 0) then None else Some term)
  in
  match addition_terms with
  | [] -> zero output_width
  | _ ->
    addition_terms
    |> Signal.tree ~arity:2 ~f:(reduce ~f:Uop.( +: ))
    |> Fn.flip uresize output_width
;;

let long_multiplication_with_subtraction
    (type a)
    (module Comb : Comb.S with type t = a)
    ~pivot
    big
  =
  let open Comb in
  (* The idea is to represent some multiplications as subtractions, namely:
   *
   * c = a * b
   *   = a * (2^n - 1 - b1 - b2 - b3)
   *   = (a << n) - a - (a << b1) - (a << b2)
   *
   * This is used to optimized multiplication by constants with most of their
   * bits set.
   *)
  let output_width = width pivot + width big in
  let subtraction_terms =
    List.filter_mapi (bits_lsb pivot) ~f:(fun i b ->
        let term = mux2 b (zero (i + width big)) (concat_msb_e [ big; zero i ]) in
        if is_vdd (term ==:. 0) then None else Some term)
  in
  List.fold
    ~init:(big @: zero (width pivot))
    ~f:(fun acc x -> acc -: uresize x output_width)
    (uresize big output_width :: subtraction_terms)
;;

let hybrid_dsp_and_luts_umul a b =
  assert (Signal.width a = Signal.width b);
  let w = Signal.width a in
  if w <= 17
  then a *: b
  else (
    let smaller = a *: b.:[16, 0] in
    let bigger =
      long_multiplication_with_addition (module Signal) ~pivot:(drop_bottom b 17) a
    in
    let result = uresize (bigger @: zero 17) (2 * w) +: uresize smaller (2 * w) in
    assert (width result = width a + width b);
    result)
;;

let specialized_45_bit_multiply
    (type a)
    (module Comb : Comb.S with type t = a)
    ~pipe
    (x : a)
    (y : a)
  =
  let open Comb in
  let split5 a =
    assert (width a <= 45);
    match split_msb ~exact:true ~part_width:9 (uresize a 45) with
    | [ a; b; c; d; e ] -> a, b, c, d, e
    | _ -> assert false
  in
  let p1 =
    let _, _, x3, x2, x1 = split5 x in
    let _, _, _, y2, y1 = split5 y in
    (x3 @: x2 @: x1) *: (y2 @: y1)
  in
  let p2 =
    let _, _, _, x2, x1 = split5 (pipe ~n:1 x) in
    let y5, y4, y3, _, _ = split5 (pipe ~n:1 y) in
    (x2 @: x1) *: (y5 @: y4 @: y3)
  in
  let p3 =
    let x5, x4, _, _, _ = split5 (pipe ~n:2 x) in
    let _, _, y3, y2, y1 = split5 (pipe ~n:2 y) in
    (x5 @: x4) *: (y3 @: y2 @: y1)
  in
  let p4 =
    let _, _, x3, _, _ = split5 (pipe ~n:3 x) in
    let _, _, y3, _, _ = split5 (pipe ~n:3 y) in
    long_multiplication_with_addition (module Comb) ~pivot:x3 y3
  in
  let p5 =
    let x5, x4, x3, _, _ = split5 (pipe ~n:4 x) in
    let y5, y4, _, _, _ = split5 (pipe ~n:4 y) in
    (x5 @: x4 @: x3) *: (y5 @: y4)
  in
  let a1 = pipe ~n:1 p1 in
  let a2 = pipe ~n:1 Uop.(p2 +: uresize (drop_bottom a1 18) 45) in
  let a3 = pipe ~n:1 Uop.(p3 +: uresize (drop_bottom a2 9) 45) in
  let a4 = pipe ~n:1 Uop.(uresize p4 36 +: drop_bottom a3 9) in
  let a5 = pipe ~n:1 (p5 +: uresize (drop_bottom a4 9) 45) in
  concat_msb
    [ a5
    ; pipe ~n:1 (sel_bottom a4 9)
    ; pipe ~n:2 (sel_bottom a3 9)
    ; pipe ~n:3 (sel_bottom a2 9)
    ; pipe ~n:4 (sel_bottom a1 18)
    ]
  |> Fn.flip uresize (width x + width y)
;;

let create_ground_multiplier_non_constant ~clock ~enable ~config a b =
  let spec = Reg_spec.create ~clock () in
  let pipeline ~n x = if Signal.is_const x then x else pipeline ~n spec ~enable x in
  match config with
  | Config.Verilog_multiply { latency } -> pipeline ~n:latency (a *: b)
  | Config.Hybrid_dsp_and_luts { latency } ->
    (* TODO(fyquah): either annotate this with backwards retiming, or
     * balance the register stages better.
     *)
    pipeline ~n:latency (hybrid_dsp_and_luts_umul a b)
  | Config.Specialized_45_bit_multiply ->
    let pipe ~n x = pipeline ~n x in
    specialized_45_bit_multiply (module Signal) ~pipe a b
;;

let create ~clock ~enable ~config a b =
  let pipeline ~n x =
    let spec = Reg_spec.create ~clock () in
    pipeline ~n spec ~enable x
  in
  let latency = Config.latency config in
  match b with
  | Signal.Const { signal_id = _; constant = constant_b } ->
    let w = width a in
    let constant_b_popcount = Bits.to_int (Bits.popcount constant_b) in
    if constant_b_popcount <= 2
    then
      (* The hybrid multiplier needs to perform a 24 * 6 multiply using LUTs
       * anyway, so 6 sounds like a reasonable threshold where we get a net win
       * using a naive multiplication algo.
       *)
      long_multiplication_with_addition (module Signal) ~pivot:b a |> pipeline ~n:latency
    else if constant_b_popcount >= w - 1
    then
      long_multiplication_with_subtraction (module Signal) ~pivot:b a
      |> pipeline ~n:latency
    else create_ground_multiplier_non_constant ~clock ~enable ~config a b
  | _ -> create_ground_multiplier_non_constant ~clock ~enable ~config a b
;;

module For_testing = struct
  let specialized_45_bit_multiply = specialized_45_bit_multiply ~pipe:(fun ~n:_ x -> x)
end
