(** Implementation of the karatsuba multiplication algorithm. Highly based on
    https://github.com/ZcashFoundation/zcash-fpga/blob/c4c0ad918898084c73528ca231d025e36740d40c/ip_cores/util/src/rtl/adder_pipe.sv

    See https://en.wikipedia.org/wiki/Karatsuba_algorithm for more details for
    the algorithm.

    The algorithm expresses the two numbers to be multiplied as:

    x = 2^(w/2) * x0 + x1
    y = 2^(w/2) * y0 + y1

    In every recursive step, B=2, and w = width of the inputs. It is required
    that width a = width b, and is an even number.

    Naively expanding the terms above yields the following:

    x * y = z0 * 2^(w)
            + z1 * 2^(w/2)
            + z2 

    where

    z0 = x0*y0
    z1 = x0*y1 + x1*y0
    z2 = x1*y1

    We can express z1 as follows: (Note that this is a slightly different
    formulation from those available in wikipedia)

    z1 = (x0 - x1)(y1 - y0) + x0*y0 + x1*y1
       = (x0 - x1)(y1 - y0) + z0 + z2

    These intermediate multiplication results will be refered to m{0,1,2}:

    m0 = z0
    m1 = (x0 - x1)(y1 - y0)
    m2 = z2

    The result of [x * y] can be computed by summing out the [m0, m1 and m2]
    terms as follows:

    m0 * 2^w
    + (m0 + m2 + m1) * 2^(w/2)
    + m2
*)

open Base
open Hardcaml
open Signal
open Reg_with_enable

module Config = struct
  type t =
    | Ground_multiplier of ground_multiplier
    | Karatsubsa_ofman_stage of karatsubsa_ofman_stage

  and karatsubsa_ofman_stage =
    { post_adder_stages : int
    ; config_m0 : t
    ; config_m1 : t
    ; config_m2 : t
    }

  and ground_multiplier =
    | Verilog_multiply of { latency : int }
    | Hybrid_dsp_and_luts of { latency : int }

  (* TODO(fyquah): Consider making this configurable. *)
  let pre_compare_stages = 1

  let rec latency (t : t) =
    match t with
    | Ground_multiplier ground_multiplier -> ground_multiplier_latency ground_multiplier
    | Karatsubsa_ofman_stage karatsubsa_ofman_stage ->
      karatsubsa_ofman_stage_latency karatsubsa_ofman_stage

  and karatsubsa_ofman_stage_latency
      { post_adder_stages; config_m0; config_m1; config_m2 }
    =
    pre_compare_stages
    + Int.max (Int.max (latency config_m0) (latency config_m1)) (latency config_m2)
    + post_adder_stages

  and ground_multiplier_latency = function
    | Verilog_multiply { latency } -> latency
    | Hybrid_dsp_and_luts { latency } -> latency
  ;;

  let rec generate ~ground_multiplier ~depth =
    match depth with
    | 0 -> Ground_multiplier ground_multiplier
    | _ ->
      let child = generate ~ground_multiplier ~depth:(depth - 1) in
      let post_adder_stages =
        match depth with
        | 4 -> 3
        | 3 -> 3
        | 2 -> 1
        | 1 -> 1
        | _ -> assert false
      in
      Karatsubsa_ofman_stage
        { post_adder_stages; config_m0 = child; config_m1 = child; config_m2 = child }
  ;;
end

let ( << ) a b = sll a b

type m_terms =
  { m0 : Signal.t
  ; m1 : Signal.t
  ; m2 : Signal.t
  }

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

module Abs_diff = struct
  type t =
    { x_lt_y : Signal.t
    ; abs_diff : Signal.t
    }
  [@@deriving fields]
end

let get_abs_diff ~scope ~clock ~enable x y =
  if Signal.is_const x && Signal.is_const y
  then { Abs_diff.x_lt_y = x <: y; abs_diff = mux2 (x <: y) (y -: x) (x -: y) }
  else (
    let x_minus_y =
      Adder_subtractor_pipe.hierarchical
        ~stages:1
        ~scope
        ~enable
        ~clock
        { lhs = x; rhs_list = [ { op = `Sub; term = y } ] }
      |> List.last_exn
    in
    let y_minus_x =
      Adder_subtractor_pipe.hierarchical
        ~stages:1
        ~scope
        ~enable
        ~clock
        { lhs = y; rhs_list = [ { op = `Sub; term = x } ] }
      |> List.last_exn
    in
    let x_lt_y = x_minus_y.carry in
    let abs_diff = mux2 x_lt_y y_minus_x.result x_minus_y.result in
    { Abs_diff.x_lt_y; abs_diff })
;;

let rec create_recursive
    ~scope
    ~clock
    ~enable
    ~(config : Config.t)
    (a : Signal.t)
    (b : Signal.t)
  =
  let ( -- ) = Scope.naming scope in
  let a = a -- "in_a" in
  let b = b -- "in_b" in
  let wa = width a in
  let wb = width b in
  if wa <> wb
  then
    raise_s
      [%message
        "Width of [a] and [b] argument of karatsuba ofman multiplier mismatch"
          (wa : int)
          (wb : int)];
  match config with
  | Karatsubsa_ofman_stage config ->
    create_karatsuba_ofman_stage ~scope ~clock ~enable ~config a b
  | Ground_multiplier config -> create_ground_multiplier ~clock ~enable ~config a b

and create_ground_multiplier ~clock ~enable ~config a b =
  let pipeline ~n x =
    let spec = Reg_spec.create ~clock () in
    pipeline ~n spec ~enable x
  in
  let latency = Config.ground_multiplier_latency config in
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

and create_ground_multiplier_non_constant ~clock ~enable ~config a b =
  let spec = Reg_spec.create ~clock () in
  let pipeline ~n x = pipeline ~n spec ~enable x in
  match config with
  | Config.Verilog_multiply { latency } -> pipeline ~n:latency (a *: b)
  | Config.Hybrid_dsp_and_luts { latency } ->
    (* TODO(fyquah): either annotate this with backwards retiming, or
     * balance the register stages better.
     *)
    pipeline ~n:latency (hybrid_dsp_and_luts_umul a b)

and create_karatsuba_ofman_stage
    ~scope
    ~clock
    ~enable
    ~config
    (a : Signal.t)
    (b : Signal.t)
  =
  let { Config.config_m0; config_m1; config_m2; post_adder_stages } = config in
  let wa = width a in
  let spec = Reg_spec.create ~clock () in
  let reg x = if Signal.is_const x then x else reg spec ~enable x in
  let pipeline ~n x = if Signal.is_const x then x else pipeline ~enable spec x ~n in
  let hw = (width a + 1) / 2 in
  let w = hw * 2 in
  let top_half x =
    (* [uresize] here is necessary to handle cases where [wa] is odd. *)
    uresize (drop_bottom x hw) hw
  in
  let btm_half x = Signal.sel_bottom x hw in
  let a' = reg a in
  let b' = reg b in
  let da = get_abs_diff ~scope ~clock ~enable (btm_half a) (top_half a) in
  let db = get_abs_diff ~scope ~clock ~enable (top_half b) (btm_half b) in
  let sign =
    pipeline
      ~n:
        (Config.karatsubsa_ofman_stage_latency config
        - post_adder_stages
        - Config.pre_compare_stages)
      (da.x_lt_y ^: db.x_lt_y)
  in
  let { m0; m1; m2 } =
    let a0 = da |> Abs_diff.abs_diff |> Fn.flip (Scope.naming scope) "a0" in
    let a1 = db |> Abs_diff.abs_diff |> Fn.flip (Scope.naming scope) "a1" in
    let recurse config subscope x y =
      let scope = Scope.sub_scope scope subscope in
      create_recursive ~scope ~enable ~clock ~config x y
    in
    let m0 = recurse config_m0 "m0" (top_half a') (top_half b') in
    let m2 = recurse config_m2 "m2" (btm_half a') (btm_half b') in
    let m1 = recurse config_m1 "m1" a0 a1 in
    { m0; m1; m2 }
  in
  assert (width m0 = w);
  assert (width m1 = w);
  assert (width m2 = w);
  (* Last step is to compute
   * [(m0 << w) + ((m0 + m2 + (sign * m1)) << hw) + m2]
   *
   * Notice that the bottom [hw] bits of [m2] will be the bottom [hw] bits of
   * the output, so it doesn't need to go through the adder.
   *)
  let btm_half_m2_pipe = pipeline ~n:config.post_adder_stages (btm_half m2) in
  let final_adder_raw =
    Adder_subtractor_pipe.hierarchical
      ~stages:config.post_adder_stages
      ~scope
      ~enable
      ~clock
      { lhs = uresize m0 (w + hw) << hw
      ; rhs_list =
          [ uresize m0 (w + hw)
          ; uresize m2 (w + hw)
          ; repeat sign (w + hw) ^: uresize m1 (w + hw)
          ; uresize sign (w + hw)
          ; uresize (top_half m2) (w + hw)
          ]
          |> List.map ~f:(fun term ->
                 { Adder_subtractor_pipe.Term_and_op.term; op = `Add })
      }
    |> List.last_exn
    |> Adder_subtractor_pipe.Single_op_output.result
  in
  uresize (final_adder_raw @: btm_half_m2_pipe) (2 * wa) -- "out"
;;

let create ?(enable = vdd) ~config ~scope ~clock a b : Signal.t =
  create_recursive ~config ~scope ~enable ~clock a b
;;

module With_interface (M : sig
  val bits : int
end) =
struct
  open M

  module I = struct
    type 'a t =
      { clock : 'a
      ; enable : 'a
      ; valid : 'a [@rtlname "in_valid"]
      ; a : 'a [@bits bits]
      ; b : 'a [@bits bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { c : 'a [@bits bits * 2]
      ; valid : 'a [@rtlname "out_valid"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create ~config (scope : Scope.t) { I.clock; enable; a; b; valid } =
    { O.c = create ~config ~clock ~enable ~scope a b
    ; valid =
        pipeline ~n:(Config.latency config) (Reg_spec.create ~clock ()) ~enable valid
    }
  ;;

  let hierarchical ~config scope (input : _ I.t) : _ O.t =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical
      ~scope
      ~name:(Printf.sprintf "karatsuba_ofman_mult_%d" bits)
      (create ~config)
      input
  ;;
end

module With_interface_multiply_constant (M : sig
  val bits : int
end) =
struct
  open M

  module I = struct
    type 'a t =
      { clock : 'a
      ; enable : 'a
      ; valid : 'a [@rtlname "in_valid"]
      ; x : 'a [@bits bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { y : 'a [@bits bits * 2]
      ; valid : 'a [@rtlname "out_valid"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create ~config ~multiply_by (scope : Scope.t) { I.clock; enable; x; valid } =
    { O.y = create ~config ~clock ~enable ~scope x (Signal.of_z ~width:bits multiply_by)
    ; valid =
        pipeline ~n:(Config.latency config) (Reg_spec.create ~clock ()) ~enable valid
    }
  ;;

  let hierarchical ~config ~multiply_by scope (input : _ I.t) : _ O.t =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical
      ~scope
      ~name:(Printf.sprintf "karatsuba_ofman_mult_%d_by_constant" bits)
      (create ~config ~multiply_by)
      input
  ;;
end

let hierarchical ~enable ~config ~scope ~clock a b =
  let bits = Signal.width a in
  match b with
  | `Constant multiply_by ->
    let module M =
      With_interface_multiply_constant (struct
        let bits = bits
      end)
    in
    let o =
      M.hierarchical ~config ~multiply_by scope { clock; enable; valid = vdd; x = a }
    in
    o.y
  | `Signal b ->
    let module M =
      With_interface (struct
        let bits = bits
      end)
    in
    let o = M.hierarchical ~config scope { clock; enable; valid = vdd; a; b } in
    o.c
;;

module For_testing = struct
  let long_multiplication_with_addition = long_multiplication_with_addition
  let long_multiplication_with_subtraction = long_multiplication_with_subtraction
end
