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
  module Level = struct
    type t =
      { radix : Radix.t
      ; pre_adder_stages : int
      ; post_adder_stages : int
      }
    [@@deriving sexp_of]
  end

  type t =
    | Ground_multiplier of Ground_multiplier.Config.t
    | Karatsubsa_ofman_stage of karatsubsa_ofman_stage

  and karatsubsa_ofman_stage =
    { level : Level.t
    ; child_config : t
    }

  let rec latency (t : t) =
    match t with
    | Ground_multiplier ground_multiplier ->
      Ground_multiplier.Config.latency ground_multiplier
    | Karatsubsa_ofman_stage karatsubsa_ofman_stage ->
      karatsubsa_ofman_stage_latency karatsubsa_ofman_stage

  and karatsubsa_ofman_stage_latency
      { level = { pre_adder_stages; post_adder_stages; radix = _ }; child_config }
    =
    pre_adder_stages + latency child_config + post_adder_stages
  ;;

  let rec generate ~ground_multiplier (levels : Level.t list) =
    match levels with
    | [] -> Ground_multiplier ground_multiplier
    | hd :: tl ->
      let child_config = generate ~ground_multiplier tl in
      Karatsubsa_ofman_stage { level = hd; child_config }
  ;;
end

let ( << ) a b = sll a b

type m_terms =
  { z0 : Signal.t
  ; m1 : Signal.t
  ; z2 : Signal.t
  }

type abs_diff =
  { sign : Signal.t
  ; value : Signal.t
  }

type radix3_terms =
  { t4 : Signal.t
  ; t3 : Signal.t
  ; t2 : Signal.t
  ; t1 : Signal.t
  ; t0 : Signal.t
  }

let abs_diff_and_sign a b =
  let sign = a <: b in
  { sign; value = mux2 sign (b -: a) (a -: b) }
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
  | Ground_multiplier config -> Ground_multiplier.create ~clock ~enable ~config a b

and create_karatsuba_ofman_stage
    ~scope
    ~clock
    ~enable
    ~(config : Config.karatsubsa_ofman_stage)
    x
    y
  =
  match config.level.radix with
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
  let ( -- ) = Scope.naming scope in
  let { Config.child_config; level = { pre_adder_stages; post_adder_stages; radix = _ } } =
    config
  in
  let pipe_add ~stages items =
    Adder_subtractor_pipe.add ~scope ~enable ~clock ~stages items
  in
  let wa = width a in
  let spec = Reg_spec.create ~clock () in
  let pipeline ~n x = if Signal.is_const x then x else pipeline ~n spec ~enable x in
  let hw = (width a + 1) / 2 in
  let w = hw * 2 in
  let top_half x =
    (* [uresize] here is necessary to handle cases where [wa] is odd. *)
    uresize (drop_bottom x hw) hw
  in
  let btm_half x = Signal.sel_bottom x hw in
  let split2 x = top_half x, btm_half x in
  let { z0; m1; z2 } =
    let recurse subscope x y =
      let scope = Scope.sub_scope scope subscope in
      create_recursive ~scope ~enable ~clock ~config:child_config x y
    in
    let a1, a0 = split2 a in
    let b1, b0 = split2 b in
    let z0 =
      recurse "m0" (pipeline ~n:pre_adder_stages a1) (pipeline ~n:pre_adder_stages b1)
    in
    let z2 =
      recurse "m2" (pipeline ~n:pre_adder_stages a0) (pipeline ~n:pre_adder_stages b0)
    in
    let m1 =
      recurse
        "m1"
        (pipe_add ~stages:pre_adder_stages [ gnd @: a1; gnd @: a0 ])
        (pipe_add ~stages:pre_adder_stages [ gnd @: b1; gnd @: b0 ])
    in
    { z0; m1; z2 }
  in
  let z1 = m1 -: uresize z2 (w + 2) -: uresize z0 (w + 2) in
  pipe_add
    ~stages:post_adder_stages
    [ uresize z0 (2 * wa) << w; uresize z1 (2 * wa) << hw; uresize z2 (2 * wa) ]
  |> Fn.flip ( -- ) "out"

and create_karatsuba_ofman_stage_radix_3
    ~scope
    ~clock
    ~enable
    ~config:
      { Config.child_config; level = { radix = _; pre_adder_stages; post_adder_stages } }
    (x : Signal.t)
    (y : Signal.t)
  =
  let ( -- ) = Scope.naming scope in
  let wx = Signal.width x in
  let spec = Reg_spec.create ~clock () in
  let pipe_add ~stages items =
    Adder_subtractor_pipe.add ~scope ~enable ~clock ~stages items
  in
  let pipeline ~n x = if Signal.is_const x then x else pipeline ~enable spec x ~n in
  let part_width = Int.round_up ~to_multiple_of:3 (width x) / 3 in
  let split3 xs =
    match List.rev (split_lsb ~exact:false ~part_width xs) with
    | [ a; b; c ] -> uresize a part_width, uresize b part_width, uresize c part_width
    | _ -> assert false
  in
  let x2, x1, x0 = split3 x in
  let y2, y1, y0 = split3 y in
  let recurse subscope x y =
    assert (width x = width y);
    let scope = Scope.sub_scope scope subscope in
    let res = create_recursive ~scope ~enable ~clock ~config:child_config x y in
    uresize res (2 * wx)
  in
  let sum_or_delta =
    (* CR-someday fyquah: Make this configurable. *)
    (* Thise choice of [sum_or_delta] overfits the [ 3; 3; 2 ] build
     * configuration. The principled thing to do is to make [sum_or_delta]
     * part of the field of the karatsuba ofman tree
     *)
    if wx >= 370 then `Sum else `Delta
  in
  let recurse_on_sum_or_delta subscope (x1, x0) (y1, y0) =
    match sum_or_delta with
    | `Sum ->
      recurse
        subscope
        (pipe_add ~stages:pre_adder_stages [ gnd @: x1; gnd @: x0 ])
        (pipe_add ~stages:pre_adder_stages [ gnd @: y1; gnd @: y0 ])
    | `Delta ->
      let { sign = signx; value = dx } = abs_diff_and_sign x1 x0 in
      let { sign = signy; value = dy } = abs_diff_and_sign y1 y0 in
      let sign =
        pipeline ~n:(Config.latency child_config + pre_adder_stages) (signx ^: signy)
      in
      let value =
        recurse
          subscope
          (pipeline ~n:pre_adder_stages dx)
          (pipeline ~n:pre_adder_stages dy)
      in
      mux2 sign (negate value) value
  in
  let d21 = recurse_on_sum_or_delta "d21" (x2, x1) (y2, y1) in
  let d10 = recurse_on_sum_or_delta "d10" (x1, x0) (y1, y0) in
  let d20 = recurse_on_sum_or_delta "d20" (x2, x0) (y2, y0) in
  let p22 =
    recurse "p22" (pipeline ~n:pre_adder_stages x2) (pipeline ~n:pre_adder_stages y2)
  in
  let p11 =
    recurse "p11" (pipeline ~n:pre_adder_stages x1) (pipeline ~n:pre_adder_stages y1)
  in
  let p00 =
    recurse "p00" (pipeline ~n:pre_adder_stages x0) (pipeline ~n:pre_adder_stages y0)
  in
  let { t4; t3; t2; t1; t0 } =
    match sum_or_delta with
    | `Sum ->
      let t4 = p22 in
      let t3 = d21 -: p22 -: p11 in
      let t2 = d20 -: p22 -: p00 +: p11 in
      let t1 = d10 -: p11 -: p00 in
      let t0 = p00 in
      { t4; t3; t2; t1; t0 }
    | `Delta ->
      let t4 = p22 in
      let t3 = p22 +: p11 -: d21 in
      let t2 = p22 +: p11 +: p00 -: d20 in
      let t1 = p11 +: p00 -: d10 in
      let t0 = p00 in
      { t4; t3; t2; t1; t0 }
  in
  pipe_add
    ~stages:post_adder_stages
    [ sll (uresize t4 (2 * wx)) (4 * part_width)
    ; sll (uresize t3 (2 * wx)) (3 * part_width)
    ; sll (uresize t2 (2 * wx)) (2 * part_width)
    ; sll (uresize t1 (2 * wx)) part_width
    ; uresize t0 (2 * wx)
    ]
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
