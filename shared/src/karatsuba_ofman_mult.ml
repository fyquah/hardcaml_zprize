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
    | Ground_multiplier of Ground_multiplier.Config.t
    | Karatsubsa_ofman_stage of karatsubsa_ofman_stage

  and karatsubsa_ofman_stage =
    { post_adder_stages : int
    ; radix : Radix.t
    ; child_config : t
    }

  (* TODO(fyquah): Consider making this configurable. *)
  let pre_compare_stages = 1

  let rec latency (t : t) =
    match t with
    | Ground_multiplier ground_multiplier ->
      Ground_multiplier.Config.latency ground_multiplier
    | Karatsubsa_ofman_stage karatsubsa_ofman_stage ->
      karatsubsa_ofman_stage_latency karatsubsa_ofman_stage

  and karatsubsa_ofman_stage_latency { post_adder_stages; child_config; radix = _ } =
    pre_compare_stages + latency child_config + post_adder_stages
  ;;

  let rec generate ~ground_multiplier radixes =
    match radixes with
    | [] -> Ground_multiplier ground_multiplier
    | hd :: tl ->
      let child_config = generate ~ground_multiplier tl in
      Karatsubsa_ofman_stage { post_adder_stages = 1; radix = hd; child_config }
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

type abs_diff =
  { sign : Signal.t
  ; value : Signal.t
  }

let abs_diff_and_sign a b =
  let sign = a <: b in
  { sign; value = mux2 sign (b -: a) (a -: b) }
;;

let abs_diff a b = (abs_diff_and_sign a b).value

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
  let latency = Ground_multiplier.Config.latency config in
  match b with
  | Signal.Const { signal_id = _; constant = constant_b } ->
    let w = width a in
    let constant_b_popcount = Bits.to_int (Bits.popcount constant_b) in
    if constant_b_popcount <= 6
    then
      (* The hybrid multiplier needs to perform a 24 * 6 multiply using LUTs
       * anyway, so 6 sounds like a reasonable threshold where we get a net win
       * using a naive multiplication algo.
       *)
      long_multiplication_with_addition (module Signal) ~pivot:b a |> pipeline ~n:latency
    else if constant_b_popcount >= w - 5
    then
      long_multiplication_with_subtraction (module Signal) ~pivot:b a
      |> pipeline ~n:latency
    else create_ground_multiplier_non_constant ~clock ~enable ~config a b
  | _ -> create_ground_multiplier_non_constant ~clock ~enable ~config a b

and create_ground_multiplier_non_constant ~clock ~enable ~config a b =
  Ground_multiplier.create ~clock ~enable ~config a b

and create_karatsuba_ofman_stage
    ~scope
    ~clock
    ~enable
    ~(config : Config.karatsubsa_ofman_stage)
    x
    y
  =
  match config.radix with
  | Radix_2 -> create_karatsuba_ofman_stage_radix_2 ~scope ~clock ~enable ~config x y
  | Radix_3 -> create_karatsuba_ofman_stage_radix_3 ~scope ~clock ~enable ~config x y

and create_karatsuba_ofman_stage_radix_2
    ~scope
    ~clock
    ~enable
    ~config
    (a : Signal.t)
    (b : Signal.t)
  =
  let { Config.child_config; post_adder_stages; radix = _ } = config in
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
  let sign =
    pipeline
      ~n:(Config.karatsubsa_ofman_stage_latency config - post_adder_stages)
      ((btm_half a -- "btm_a" <: top_half a -- "top_a")
      ^: (top_half b -- "top_b" <: btm_half b -- "btm_b"))
  in
  let { m0; m1; m2 } =
    let a0 =
      abs_diff (btm_half a) (top_half a) |> reg |> Fn.flip (Scope.naming scope) "a0"
    in
    let a1 =
      abs_diff (top_half b) (btm_half b) |> reg |> Fn.flip (Scope.naming scope) "a1"
    in
    let recurse subscope x y =
      let scope = Scope.sub_scope scope subscope in
      create_recursive ~scope ~enable ~clock ~config:child_config x y
    in
    let m0 = recurse "m0" (top_half a') (top_half b') in
    let m2 = recurse "m2" (btm_half a') (btm_half b') in
    let m1 = recurse "m1" a0 a1 in
    { m0; m1; m2 }
  in
  let m0 = uresize m0 (w * 2) in
  let m1 = uresize m1 (w * 2) in
  let m2 = uresize m2 (w * 2) in
  (m0 << w) +: (m0 +: m2 +: mux2 sign (negate m1) m1 << hw) +: m2
  |> Fn.flip uresize (2 * wa)
  |> reg
  |> Fn.flip ( -- ) "out"

and create_karatsuba_ofman_stage_radix_3
    ~scope
    ~clock
    ~enable
    ~config:{ Config.child_config; post_adder_stages = _; radix = _ }
    (x : Signal.t)
    (y : Signal.t)
  =
  let wx = Signal.width x in
  let spec = Reg_spec.create ~clock () in
  let reg x = if Signal.is_const x then x else reg spec ~enable x in
  let pipeline ~n x = if Signal.is_const x then x else pipeline ~enable spec x ~n in
  let part_width = Int.round_up ~to_multiple_of:3 (width x) / 3 in
  let split3 xs =
    match split_msb ~exact:false ~part_width xs with
    | [ a; b; c ] -> uresize a part_width, uresize b part_width, uresize c part_width
    | _ -> assert false
  in
  let x2, x1, x0 = split3 x in
  let y2, y1, y0 = split3 y in
  let recurse subscope x y =
    let scope = Scope.sub_scope scope subscope in
    create_recursive ~scope ~enable ~clock ~config:child_config x y
    |> Fn.flip uresize (2 * wx)
  in
  let recurse_abs_diff subscope (x1, x0) (y1, y0) =
    let { sign = signx; value = dx } = abs_diff_and_sign x1 x0 in
    let { sign = signy; value = dy } = abs_diff_and_sign y1 y0 in
    let sign = pipeline ~n:(Config.latency child_config + 1) (signx ^: signy) in
    let value = recurse subscope (reg dx) (reg dy) in
    mux2 sign (negate value) value
  in
  let d21 = recurse_abs_diff "d21" (x2, x1) (y2, y1) in
  let d10 = recurse_abs_diff "d10" (x1, x0) (y1, y0) in
  let d20 = recurse_abs_diff "d20" (x2, x0) (y2, y0) in
  let p22 = recurse "p22" (reg x2) (reg y2) in
  let p11 = recurse "p11" (reg x1) (reg y1) in
  let p00 = recurse "p00" (reg x0) (reg y0) in
  sll p22 (4 * part_width)
  +: sll (p22 +: p11 -: d21) (3 * part_width)
  +: sll (p22 +: p11 +: p00 -: d20) (2 * part_width)
  +: sll (p11 +: p00 -: d10) part_width
  +: p00
  |> Fn.flip uresize (2 * wx)
  |> reg
  |> Fn.flip ( -- ) "out"
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
