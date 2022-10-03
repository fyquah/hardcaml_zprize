open Base
open Hardcaml
open Signal
open Reg_with_enable

module Config = struct
  module Level = struct
    type t =
      { radix : Radix.t
      ; pre_adder_stages : int
      ; middle_adder_stages : int
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
    { level = { pre_adder_stages; post_adder_stages; radix = _; middle_adder_stages }
    ; child_config
    }
    =
    pre_adder_stages + latency child_config + post_adder_stages + middle_adder_stages
  ;;

  let rec generate ~ground_multiplier (levels : Level.t list) =
    match levels with
    | [] -> Ground_multiplier ground_multiplier
    | hd :: tl ->
      let child_config = generate ~ground_multiplier tl in
      Karatsubsa_ofman_stage { level = hd; child_config }
  ;;
end

module IO (M : sig
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
    [@@deriving sexp_of, hardcaml, fields]
  end
end

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

let rec create_recursive ~scope ~clock ~enable ~config a b =
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
  let module X =
    IO (struct
      let bits = wa
    end)
  in
  let module H = Hierarchy.In_scope (X.I) (X.O) in
  let override_b = if Signal.is_const b then Some b else None in
  match config with
  | Config.Karatsubsa_ofman_stage config ->
    let name =
      Printf.sprintf
        "karatsuba_ofman_stage_%d_%s"
        wa
        (match config.level.radix with
         | Radix_2 -> "radix_2"
         | Radix_3 -> "radix_3")
    in
    H.hierarchical
      ~name
      ~scope
      (fun scope { X.I.clock; enable; valid = _; a; b } ->
        let b =
          match override_b with
          | Some x -> x
          | None -> b
        in
        (* CR-someday fyquah: Populate [valid] properly. *)
        { X.O.c = create_karatsuba_ofman_stage ~scope ~clock ~enable ~config a b
        ; valid = gnd
        })
      { clock; enable; a; b; valid = gnd }
    |> X.O.c
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
  let { Config.child_config
      ; level = { pre_adder_stages; middle_adder_stages; post_adder_stages; radix = _ }
      }
    =
    config
  in
  let spec = Reg_spec.create ~clock () in
  let pipeline ~n x = if Signal.is_const x then x else pipeline ~n spec ~enable x in
  let pipe_add ~stages items =
    Adder_subtractor_pipe.add_no_carry ~scope ~enable ~clock ~stages items
  in
  let pipe_add_sub ~n lhs rhs_list =
    Adder_subtractor_pipe.mixed_no_carry
      ~scope
      ~enable
      ~clock
      ~stages:n
      ~init:lhs
      rhs_list
  in
  let wa = width a in
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
    let pre_add a b =
      match child_config with
      | Ground_multiplier _ -> pipeline ~n:pre_adder_stages (a +: b)
      | Karatsubsa_ofman_stage _ -> pipe_add ~stages:pre_adder_stages [ a; b ]
    in
    let m1 =
      recurse "m1" (pre_add (gnd @: a1) (gnd @: a0)) (pre_add (gnd @: b1) (gnd @: b0))
    in
    { z0; m1; z2 }
  in
  let z1 =
    pipe_add_sub
      ~n:middle_adder_stages
      (uresize m1 (w + 1))
      [ Sub (uresize z2 (w + 1)); Sub (uresize z0 (w + 1)) ]
  in
  let z0 = pipeline ~n:middle_adder_stages z0 in
  let z2 = pipeline ~n:middle_adder_stages z2 in
  (* The following computes
   * [let o = (z0 << w) + (z1 << hw) + z2], where [z1 = m1 - z2 - z0] in a
   * convoluted way.
   *
   * A possible optimization here is to start computing [o] right when we have
   * partial lower bits results of [z1]. This can save on SRL resources.
   *)
  let o =
    let o0 = pipeline ~n:post_adder_stages (sel_bottom z2 hw) in
    let o1 =
      let d2 = drop_bottom z2 hw in
      assert (width d2 <= w - hw);
      (* We need to compute add d2 at some point down the chain, but since z0
       * and d2's bits never overlap, we can just or z0 and z2 together
       * (with appropriate zeros) to compute the addition.
       *)
      pipe_add_sub
        ~n:post_adder_stages
        (uresize (concat_msb_e [ z0; zero (w - hw - width d2); d2 ]) ((2 * wa) - hw))
        [ Add (uresize z1 ((2 * wa) - hw)) ]
    in
    o1 @: o0
  in
  o -- "out"

and create_karatsuba_ofman_stage_radix_3
  ~scope
  ~clock
  ~enable
  ~config:
    { Config.child_config
    ; level = { radix = _; pre_adder_stages; middle_adder_stages; post_adder_stages }
    }
  (x : Signal.t)
  (y : Signal.t)
  =
  let ( -- ) = Scope.naming scope in
  let wx = Signal.width x in
  let spec = Reg_spec.create ~clock () in
  let pipe_add ~stages items =
    Adder_subtractor_pipe.add_no_carry ~scope ~enable ~clock ~stages items
  in
  let pipe_add_sub ~n lhs rhs_list =
    Adder_subtractor_pipe.mixed_no_carry
      ~scope
      ~enable
      ~clock
      ~stages:n
      ~init:lhs
      rhs_list
  in
  let pipeline ~n x =
    assert (n >= 0);
    if Signal.is_const x then x else pipeline ~enable spec x ~n
  in
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
    create_recursive ~scope ~enable ~clock ~config:child_config x y
  in
  let sum_or_delta =
    (* CR-someday fyquah: Make this configurable. *)
    (* Thise choice of [sum_or_delta] overfits the [ 3; 3; 2 ] build
     * configuration. The principled thing to do is to make [sum_or_delta]
     * part of the field of the karatsuba ofman tree
     *)
    if wx >= 370 then `Sum else if wx <= 126 then `Sum else `Delta
  in
  let wd =
    match sum_or_delta with
    | `Sum -> 2 * (part_width + 1)
    | `Delta -> (2 * part_width) + 1
  in
  let wp = 2 * part_width in
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
      let value = gnd @: value in
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
  assert (width d21 = wd);
  assert (width d10 = wd);
  assert (width d20 = wd);
  assert (width p22 = wp);
  assert (width p11 = wp);
  assert (width p00 = wp);
  let wt3 =
    (* [t3] computes (x1y2 + x2y1) *)
    (2 * part_width) + 1
  in
  let wt2 =
    (* [t2] computes (x0y2 + x2y0 + x1y1) *)
    (2 * part_width) + 2
  in
  let wt1 =
    (* [t1] computes (x0y1 + x1y0) *)
    wt3
  in
  let { t4; t3; t2; t1; t0 } =
    match sum_or_delta with
    | `Sum ->
      let t4 = pipeline ~n:middle_adder_stages p22 in
      let t3 =
        pipe_add_sub
          ~n:middle_adder_stages
          (sresize d21 wt3)
          [ Sub (uresize p22 wt3); Sub (uresize p11 wt3) ]
      in
      let t2 =
        pipe_add_sub
          ~n:middle_adder_stages
          (sresize d20 wt2)
          [ Sub (uresize p22 wt2); Sub (uresize p00 wt2); Add (uresize p11 wt2) ]
      in
      let t1 =
        pipe_add_sub
          ~n:middle_adder_stages
          (sresize d10 wt1)
          [ Sub (uresize p11 wt1); Sub (uresize p00 wt1) ]
      in
      let t0 = pipeline ~n:middle_adder_stages p00 in
      { t4; t3; t2; t1; t0 }
    | `Delta ->
      let t4 = pipeline ~n:middle_adder_stages p22 in
      let t3 =
        pipe_add_sub
          ~n:middle_adder_stages
          (uresize p22 wt3)
          [ Add (uresize p11 wt3); Sub (uresize d21 wt3) ]
      in
      let t2 =
        pipe_add_sub
          ~n:middle_adder_stages
          (uresize p22 wt2)
          [ Add (uresize p11 wt2); Add (uresize p00 wt2); Sub (sresize d20 wt2) ]
      in
      let t1 =
        pipe_add_sub
          ~n:middle_adder_stages
          (uresize p11 wt1)
          [ Add (uresize p00 wt1); Sub (sresize d10 wt1) ]
      in
      let t0 = pipeline ~n:middle_adder_stages p00 in
      { t4; t3; t2; t1; t0 }
  in
  let result =
    let o0 = pipeline ~n:post_adder_stages (sel_bottom t0 part_width) in
    let o1 =
      let d0 = drop_bottom t0 part_width in
      assert (width d0 <= (2 * part_width) - part_width);
      pipe_add
        ~stages:post_adder_stages
        [ sll (uresize t4 ((2 * wx) - part_width)) ((4 * part_width) - part_width)
        ; sll (uresize t3 ((2 * wx) - part_width)) ((3 * part_width) - part_width)
        ; uresize
            (concat_msb_e [ t2; zero ((2 * part_width) - part_width - width d0); d0 ])
            ((2 * wx) - part_width)
        ; sll (uresize t1 ((2 * wx) - part_width)) ((1 * part_width) - part_width)
        ]
    in
    o1 @: o0
  in
  result -- "out"
;;

let create ?(enable = vdd) ~config ~scope ~clock a b : Signal.t =
  create_recursive ~config ~scope ~enable ~clock a b
;;

module With_interface (M : sig
  val bits : int
end) =
struct
  open M
  include IO (M)

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
