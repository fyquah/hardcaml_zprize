open Base
open Hardcaml
open Signal

module Config = struct
  type t =
    | Verilog_multiply of { latency : int }
    | Hybrid_dsp_and_luts of
        { latency : int
        ; lut_only_hamming_weight_threshold : int
        }
    | Mixed of
        { latency : int
        ; lut_only_hamming_weight_threshold : int option
        ; hybrid_hamming_weight_threshold : int option
        }
    | Specialized_43_bit_multiply
  [@@deriving sexp_of]

  let latency = function
    | Verilog_multiply { latency } -> latency
    | Hybrid_dsp_and_luts { latency; lut_only_hamming_weight_threshold = _ } -> latency
    | Specialized_43_bit_multiply -> 5
    | Mixed { latency; _ } -> latency
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

let opt_to_bits (x : Signal.t) =
  match x with
  | Signal.Const { constant; signal_id = _ } -> Some constant
  | _ -> None
;;

let long_multiplication_with_addition_for_signal ~pivot big =
  let open Signal in
  let output_width = width pivot + width big in
  let addition_terms =
    List.filter_mapi (bits_lsb pivot) ~f:(fun i b ->
      if is_vdd (b ==:. 0)
      then None
      else Some (With_shift.create ~shift:i (mux2 b big (zero (width big)))))
  in
  match opt_to_bits pivot with
  | None ->
    Signal.add_attribute
      (big *: pivot)
      (Rtl_attribute.create ~value:(String "no") "USE_DSP")
  | Some pivot ->
    let pivot = Naf.of_bits pivot in
    (match
       Naf.bits_lsb pivot
       |> List.filter_mapi ~f:(fun shift bit ->
            match bit with
            | Naf.Bit.Zero -> None
            | Naf.Bit.Pos_one -> Some (`Add (With_shift.create ~shift big))
            | Naf.Bit.Neg_one -> Some (`Sub (With_shift.create ~shift big)))
       |> List.rev
     with
     | [] -> zero output_width
     | [ hd ] ->
       (match hd with
        | `Add hd -> With_shift.to_signal (With_shift.uresize hd output_width)
        | `Sub hd ->
          assert (With_shift.width hd = Signal.width big);
          With_shift.mixed
            ~init:(With_shift.create ~shift:(Naf.width pivot) big)
            [ `Sub (With_shift.uresize hd (Naf.width pivot + Signal.width big)) ]
          |> Fn.flip With_shift.uresize output_width
          |> With_shift.to_signal)
     | init :: items ->
       let addition_term_bits =
         let maximum_addition_term_width =
           List.map addition_terms ~f:With_shift.width
           |> List.max_elt ~compare:Int.compare
           |> Option.value_exn
         in
         let num_terms = List.length addition_terms in
         maximum_addition_term_width + Int.ceil_log2 num_terms
       in
       let init =
         match init with
         | `Add x -> With_shift.uresize x addition_term_bits
         | `Sub _ -> assert false
       in
       let items =
         List.map items ~f:(fun x ->
           match x with
           | `Add x -> `Add (With_shift.uresize x addition_term_bits)
           | `Sub x -> `Sub (With_shift.uresize x addition_term_bits))
       in
       With_shift.uresize (With_shift.mixed ~init items) output_width
       |> With_shift.to_signal)
;;

let should_multiply_top_with_luts b =
  match b with
  | Signal.Const { constant = b; signal_id = _ } ->
    let bottom = Naf.of_bits (Bits.drop_top b 17) in
    let top = Naf.of_bits (Bits.drop_bottom b 17) in
    Naf.hamming_weight top < Naf.hamming_weight bottom
  | _ -> true (* doesn't matter with non constants ... *)
;;

let hybrid_dsp_and_luts_umul a b =
  assert (Signal.width a = Signal.width b);
  let w = Signal.width a in
  if w <= 17
  then a *: b
  else (
    let result =
      if should_multiply_top_with_luts b
      then (
        let smaller = a *: sel_bottom b 17 in
        let bigger =
          let pivot = drop_bottom b 17 in
          long_multiplication_with_addition_for_signal ~pivot a
        in
        uresize (bigger @: zero 17) (2 * w) +: uresize smaller (2 * w))
      else (
        let smaller =
          let pivot = drop_top b 17 in
          long_multiplication_with_addition_for_signal ~pivot a
        in
        let bigger = a *: sel_top b 17 in
        uresize (bigger @: zero (width b - 17)) (2 * w) +: uresize smaller (2 * w))
    in
    assert (width result = width a + width b);
    result)
;;

let hybrid_dsp_and_luts_umul_maybe_lut_only ~lut_only_hamming_weight_threshold a b =
  match b with
  | Signal.Const { constant; signal_id = _ } ->
    if Naf.hamming_weight (Naf.of_bits constant) < lut_only_hamming_weight_threshold
    then (
      let result = long_multiplication_with_addition_for_signal ~pivot:b a in
      assert (Signal.width result = Signal.width a + Signal.width b);
      result)
    else hybrid_dsp_and_luts_umul a b
  | _ -> hybrid_dsp_and_luts_umul a b
;;

let mixed_multiplication
  ~lut_only_hamming_weight_threshold
  ~hybrid_hamming_weight_threshold
  a
  b
  =
  let result =
    match b with
    | Signal.Const { constant; signal_id = _ } ->
      let hamming_weight = Naf.hamming_weight (Naf.of_bits constant) in
      assert (hamming_weight >= 0);
      if hamming_weight < Option.value ~default:(-1) lut_only_hamming_weight_threshold
      then long_multiplication_with_addition_for_signal ~pivot:b a
      else if hamming_weight < Option.value ~default:(-1) hybrid_hamming_weight_threshold
      then hybrid_dsp_and_luts_umul a b
      else a *: b
    | _ -> a *: b
  in
  assert (width a + width b = width result);
  result
;;

(* Maybe delete this? Don't think this is that useful..*)
let specialized_43_bit_multiply
  (type a)
  (module Comb : Comb.S with type t = a)
  ~pipe
  (x : a)
  (y : a)
  =
  let open Comb in
  assert (width x = width y);
  assert (width x <= 43);
  let pipe2 ~n = uresize (pipe ~n x) 43, uresize (pipe ~n y) 43 in
  let p1 = x.:[25, 0] *: y.:[16, 0] in
  let p2 =
    let x, y = pipe2 ~n:1 in
    x.:[16, 0] *: y.:[42, 17]
  in
  let p3 =
    let x, y = pipe2 ~n:2 in
    x.:[42, 26] *: y.:[25, 0]
  in
  let p4 =
    let x, y = pipe2 ~n:3 in
    long_multiplication_with_addition (module Comb) ~pivot:x.:[25, 17] y.:[25, 17]
  in
  let p5 =
    let x, y = pipe2 ~n:4 in
    x.:[42, 17] *: y.:[42, 26]
  in
  assert (width p1 = 26 + 17);
  assert (width p2 = 26 + 17);
  assert (width p3 = 26 + 17);
  assert (width p4 = 18);
  assert (width p5 = 26 + 17);
  let a1 = pipe ~n:1 p1 in
  let a2 = pipe ~n:1 Uop.(p2 +: drop_bottom a1 17) in
  let a3 = pipe ~n:1 Uop.(p3 +: drop_bottom a2 9) in
  let a4 = pipe ~n:1 Uop.(p4 +: drop_bottom a3 8) in
  let a5 = pipe ~n:1 (p5 +: uresize (drop_bottom a4 9) 43) in
  let result =
    concat_msb
      [ a5
      ; pipe ~n:1 (sel_bottom a4 9)
      ; pipe ~n:2 (sel_bottom a3 8)
      ; pipe ~n:3 (sel_bottom a2 9)
      ; pipe ~n:4 (sel_bottom a1 17)
      ]
  in
  assert (width result = 43 * 2);
  uresize result (width x + width y)
;;

let create ~clock ~enable ~config a b =
  let spec = Reg_spec.create ~clock () in
  let pipeline ~n x = if Signal.is_const x then x else pipeline ~n spec ~enable x in
  match config with
  | Config.Verilog_multiply { latency } -> pipeline ~n:latency (a *: b)
  | Config.Hybrid_dsp_and_luts { latency; lut_only_hamming_weight_threshold } ->
    (* TODO(fyquah): either annotate this with backwards retiming, or
     * balance the register stages better.
     *)
    pipeline
      ~n:latency
      (hybrid_dsp_and_luts_umul_maybe_lut_only ~lut_only_hamming_weight_threshold a b)
  | Config.Specialized_43_bit_multiply ->
    let pipe ~n x = pipeline ~n x in
    specialized_43_bit_multiply (module Signal) ~pipe a b
  | Config.Mixed
      { latency; lut_only_hamming_weight_threshold; hybrid_hamming_weight_threshold } ->
    (* TODO(fyquah): We are at the mercy of retiming here. *)
    mixed_multiplication
      ~lut_only_hamming_weight_threshold
      ~hybrid_hamming_weight_threshold
      a
      b
    |> pipeline ~n:latency
;;

module For_testing = struct
  let long_multiplication_with_addition = long_multiplication_with_addition
  let specialized_43_bit_multiply = specialized_43_bit_multiply ~pipe:(fun ~n:_ x -> x)
end
