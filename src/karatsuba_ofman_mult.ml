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
    | Ground_multiplier ground_multiplier ->
      ground_multiplier_latency ground_multiplier
    | Karatsubsa_ofman_stage karatsubsa_ofman_stage ->
      karatsubsa_ofman_stage_latency karatsubsa_ofman_stage

  and karatsubsa_ofman_stage_latency { post_adder_stages; config_m0; config_m1; config_m2 } = 
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
      Karatsubsa_ofman_stage
        { post_adder_stages = 1
        ; config_m0 = child
        ; config_m1 = child
        ; config_m2 = child
        }
  ;;
end

let (<<) a b = sll a b 

type m_terms =
  { m0 : Signal.t
  ; m1 : Signal.t
  ; m2 : Signal.t
  }

let lut_and_carry_multiply ~pivot big =
  List.mapi (Signal.bits_lsb pivot) ~f:(fun i b ->
      mux2 b
        (concat_msb_e [ big; zero i ])
        (zero (i + width big)))
  |> Signal.tree ~arity:2 ~f:(reduce ~f:Uop.( +: ))
;;

let hybrid_dsp_and_luts_umul a b =
  assert (Signal.width a = Signal.width b);
  let w = Signal.width a in
  if w <= 17 then
    a *: b
  else
    let smaller = a *: b.:[16, 0] in
    let bigger = lut_and_carry_multiply ~pivot:(drop_bottom b 17) a in
    let result = uresize (bigger @: zero 17) (2 * w) +: uresize smaller (2 * w) in
    assert (width result = width a + width b);
    result
;;

let rec create_recursive
    ~scope
    ~clock
    ~enable
    ~(config : Config.t)
    (a : Signal.t)
    (b : Signal.t) =
  let (--) = Scope.naming scope in
  let a = a -- "in_a" in
  let b = b -- "in_b" in
  let wa = width a in
  let wb = width b in
  if wa <> wb then (
    raise_s [%message
      "Width of [a] and [b] argument of karatsuba ofman multiplier mismatch"
        (wa : int)
        (wb : int)
    ]
  );
  match config with 
  | Karatsubsa_ofman_stage config ->
    create_karatsuba_ofman_stage ~scope ~clock ~enable ~config a b
  | Ground_multiplier config ->
    create_ground_multiplier ~clock ~enable ~config a b

and create_ground_multiplier ~clock ~enable ~config a b =
  (* TODO(fyquah): Special-case multiplication by constants. *)
  let spec = Reg_spec.create ~clock () in
  let pipeline ~n x = pipeline ~n spec ~enable x in
  match config with
  | Config.Verilog_multiply { latency } -> 
    pipeline ~n:latency (a *: b)
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
    (b : Signal.t) =
  let { Config. config_m0; config_m1; config_m2; post_adder_stages } = config in
  let wa = width a in
  let spec = Reg_spec.create ~clock () in
  let reg x = reg spec ~enable x in
  let pipeline ~n x = pipeline ~enable spec x ~n in
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
      (((btm_half a -- "btm_a") <: (top_half a -- "top_a"))
       ^: ((top_half b -- "top_b") <: (btm_half b -- "btm_b")))
  in
  let { m0; m1; m2 } =
    let a0 =
      mux2 (btm_half a >: top_half a)
        (btm_half a -: top_half a)
        (top_half a -: btm_half a)
      |> reg
      |> Fn.flip (Scope.naming scope) "a0"
    in
    let a1 =
      mux2 (top_half b >: btm_half b)
        (top_half b -: btm_half b)
        (btm_half b -: top_half b)
      |> reg
      |> Fn.flip (Scope.naming scope) "a1"
    in
    let recurse config subscope x y =
      let scope = Scope.sub_scope scope subscope in
      create_recursive ~scope ~enable ~clock ~config x y
    in
    let m0 = recurse config_m0 "m0" (top_half a') (top_half b') in
    let m2 = recurse config_m2 "m2" (btm_half a') (btm_half b') in
    let m1 = recurse config_m1 "m1" a0 a1 in
    { m0; m1; m2 }
  in
  let m0 = uresize m0 (w * 2) in
  let m1 = uresize m1 (w * 2) in
  let m2 = uresize m2 (w * 2) in
  ((m0 << w)
   +: ((m0
        +: m2
        +: (mux2 sign (negate m1) m1))
       << hw)
   +: m2)
  |> Fn.flip uresize (2 * wa)
  |> reg
  |> Fn.flip (--) "out"
;;

let create ?(enable = vdd) ~config ~scope ~clock a b : Signal.t =
  create_recursive ~config ~scope ~enable ~clock a b
;;

module With_interface(M : sig val bits : int end) = struct
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

  let create ~config (scope : Scope.t) { I. clock; enable; a; b; valid }  =
    { O.c = create ~config ~clock ~enable ~scope a b
    ; valid =
        pipeline ~n:(Config.latency config) (Reg_spec.create ~clock ()) ~enable valid
    }
  ;;

  let hierarchical ~config scope (input : _ I.t) : _ O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical
      ~scope
      ~name:(Printf.sprintf "karatsuba_ofman_mult_%d" bits)
      (create ~config)
      input
  ;;
end

let hierarchical ~enable ~config ~scope ~clock a b =
  let bits = Signal.width a in
  let module M = With_interface(struct let bits = bits end) in
  let o =
    M.hierarchical ~config scope
      { clock
      ; enable
      ; valid = vdd
      ; a
      ; b
      }
  in
  o.c
;;
