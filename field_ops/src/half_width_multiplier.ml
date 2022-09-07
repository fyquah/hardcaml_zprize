open Base
open Hardcaml
open Signal
open Reg_with_enable

module Config = struct
  module Level = Karatsuba_ofman_mult.Config.Level

  type t =
    { levels : Level.t list
    ; ground_multiplier : Ground_multiplier.Config.t
    }

  let latency { levels; ground_multiplier } =
    match levels with
    | [] -> Ground_multiplier.Config.latency ground_multiplier
    | { radix = _; pre_adder_stages = _; middle_adder_stages = _; post_adder_stages }
      :: tl ->
      let slowest_multiplier =
        tl
        |> Karatsuba_ofman_mult.Config.generate ~ground_multiplier
        |> Karatsuba_ofman_mult.Config.latency
      in
      slowest_multiplier + post_adder_stages
  ;;
end

module Input = Multiplier_input

let rec create_recursive
    ~scope
    ~clock
    ~enable
    ~ground_multiplier
    ~(levels : Config.Level.t list)
    (input : Input.t)
  =
  match levels with
  | [] ->
    let a, b =
      match input with
      | Multiply_by_constant _ -> assert false
      | Multiply (a, b) -> a, b
      | Square a -> a, a
    in
    Ground_multiplier.create ~clock ~enable ~config:ground_multiplier a b
  | level :: remaining_levels ->
    create_level
      ~scope
      ~clock
      ~enable
      ~ground_multiplier
      ~remaining_levels
      ~level
      (input : Input.t)

and create_level
    ~scope
    ~clock
    ~enable
    ~ground_multiplier
    ~remaining_levels
    ~level:
      { Config.Level.radix
      ; post_adder_stages
      ; pre_adder_stages = _
      ; middle_adder_stages = _
      }
    (input : Input.t)
  =
  let spec = Reg_spec.create ~clock () in
  let pipeline ~n x = if is_const x then x else pipeline ~enable spec x ~n in
  let child_karatsuba_config =
    Karatsuba_ofman_mult.Config.generate ~ground_multiplier remaining_levels
  in
  let create_recursive input =
    create_recursive
      ~scope
      ~clock
      ~enable
      ~levels:remaining_levels
      ~ground_multiplier
      input
    |> pipeline
         ~n:
           (Karatsuba_ofman_mult.Config.latency child_karatsuba_config
           - Config.latency { levels = remaining_levels; ground_multiplier })
  in
  let create_full_multiplier a b =
    assert (Signal.width a = Signal.width b);
    match child_karatsuba_config with
    | Ground_multiplier config -> Ground_multiplier.create ~clock ~enable ~config a b
    | Karatsubsa_ofman_stage _ ->
      Karatsuba_ofman_mult.hierarchical
        ~scope
        ~config:child_karatsuba_config
        ~enable
        ~clock
        a
        (match b with
        | Const { signal_id = _; constant } ->
          `Constant (Bits.to_z ~signedness:Unsigned constant)
        | _ -> `Signal b)
  in
  let w =
    match input with
    | Multiply_by_constant _ ->
      (* The input is sanitize to either Multiply or Square. *)
      assert false
    | Multiply (a, b) ->
      if width a <> width b
      then
        raise_s
          [%message
            "Width of [a] and [b] argument of karatsuba ofman multiplier mismatch"
              (width a : int)
              (width b : int)];
      width a
    | Square s -> width s
  in
  match radix with
  | Radix.Radix_2 ->
    let hw = (w + 1) / 2 in
    let top_half x = uresize (drop_bottom x hw) hw in
    let btm_half x = sel_bottom x hw in
    let top_and_btm_half x = top_half x, btm_half x in
    (* ((ta * 2^hw) + ba) * ((tb * 2^hw) + bb)
     * = ((ua * ub) * 2^(2 * hw)) + (((ua * lb) + (ub * la)) * 2^hw) + (la * lb)
     *
     * The upper-halve is zero in a half-width hader, reach reduces the circuit
     * to the following:
     *
     * (((ua * lb) + (ub * la)) * 2^hw) + (la * lb)
     *
     * In the special case of squaring numbers, ua = la, ub = lb; we need
     * only 2 mults rather than 3.
     *)
    (match input with
    | Multiply_by_constant _ -> assert false
    | Multiply (a, b) ->
      let ua, la = top_and_btm_half a in
      let ub, lb = top_and_btm_half b in
      let ua_mult_lb = uresize (create_recursive (Multiply (ua, lb))) w in
      let ub_mult_la = uresize (create_recursive (Multiply (ub, la))) w in
      let la_mult_lb = uresize (create_full_multiplier la lb) w in
      let o =
        let o0 = pipeline ~n:post_adder_stages (sel_bottom la_mult_lb hw) in
        let o1 =
          Adder_subtractor_pipe.add_no_carry
            ~stages:post_adder_stages
            ~scope
            ~enable
            ~clock
            [ uresize ua_mult_lb (w - hw)
            ; uresize ub_mult_la (w - hw)
            ; uresize (drop_bottom la_mult_lb hw) (w - hw)
            ]
        in
        o1 @: o0
      in
      o
    | Square a ->
      let ua, la = top_and_btm_half a in
      let ua_mult_la_times_2 =
        sll (uresize (create_recursive (Multiply (ua, la))) w) (hw + 1)
      in
      let la_mult_la = uresize (create_full_multiplier la la) w in
      pipeline ~n:post_adder_stages (ua_mult_la_times_2 +: la_mult_la))
  | Radix_3 ->
    let k = Int.round_up ~to_multiple_of:3 w / 3 in
    let split3 x =
      match split_msb ~exact:true ~part_width:k (uresize x (3 * k)) with
      | [ a; b; c ] -> a, b, c
      | _ -> assert false
    in
    (match input with
    | Multiply_by_constant _ -> assert false
    | Square _ -> raise_s [%message "Half width radix-3 squarer unimplemented..."]
    | Multiply (x, y) ->
      (* ((x2 * 2^(2k)) + (x1 * 2^k) + x0)((y2 * 2^(2k)) + (y1 * 2^k) + y0)
       *  = x2y2 * 2^4k
       *    + x2y1 * 2^3k
       *    + x2y0 * 2^2k
       *    + x1y2 * 2^3k
       *    + x1y1 * 2^2k
       *    + x1y0 * 2^k
       *    + x0y2 * 2^2k
       *    + x0y1 * 2^k
       *    + x0y0
       *  = x2y2 * 2^4k
       *    + (x2y1 + x1y2) * 2^3k
       *    + (x2y0 + x1y1 + x0y2) * 2^2k
       *    + (x1y0 + x0y1) * 2^k
       *    + x0y0
       *
       * The width of every partial product is 2k, and we only need the bottom
       * 3k bits. So the above expression can be reduced to the following:
       *
       *  = (x2y0 + x1y1 + x0y2) * 2^2k <-- only half width mult in this row
       *    + (x1y0 + x0y1) * 2^k
       *    + x0y0
       *
       *)
      let x2, x1, x0 = split3 x in
      let y2, y1, y0 = split3 y in
      let x2y0 = uresize (create_recursive (Multiply (x2, y0))) w in
      let x1y1 = uresize (create_recursive (Multiply (x1, y1))) w in
      let x0y2 = uresize (create_recursive (Multiply (x0, y2))) w in
      let x1y0 = uresize (create_full_multiplier x1 y0) w in
      let x0y1 = uresize (create_full_multiplier x0 y1) w in
      let x0y0 = uresize (create_full_multiplier x0 y0) w in
      let o =
        let o0 = pipeline ~n:post_adder_stages (sel_bottom x0y0 k) in
        let o1 =
          Adder_subtractor_pipe.add_no_carry
            ~stages:post_adder_stages
            ~scope
            ~enable
            ~clock
            [ sll (uresize (x2y0 +: x1y1 +: x0y2) (w - k)) k
            ; uresize (x1y0 +: x0y1) (w - k)
            ; uresize (drop_bottom x0y0 k) (w - k)
            ]
        in
        o1 @: o0
      in
      o)
;;

let create_with_config ~config:{ Config.levels; ground_multiplier } =
  create_recursive ~levels ~ground_multiplier
;;

module type Width = sig
  val bits : int
end

module With_interface_multiply (M : Width) = struct
  include M

  module I = struct
    type 'a t =
      { clock : 'a
      ; enable : 'a
      ; x : 'a [@bits bits]
      ; y : 'a [@bits bits]
      ; in_valid : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { z : 'a [@bits bits]
      ; out_valid : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create ~config scope { I.clock; enable; x; y; in_valid } =
    let spec = Reg_spec.create ~clock () in
    let z = create_with_config ~config ~scope ~clock ~enable (Multiply (x, y)) in
    let out_valid = pipeline spec ~enable ~n:(Config.latency config) in_valid in
    assert (width z = bits);
    { O.z; out_valid }
  ;;

  let hierarchical ~name ~config scope i =
    let module H = Hierarchy.In_scope (I) (O) in
    let { O.z; out_valid = _ } = H.hierarchical ~scope ~name (create ~config) i in
    z
  ;;
end

module Interface_1arg (M : Width) = struct
  include M

  module I = struct
    type 'a t =
      { clock : 'a
      ; enable : 'a
      ; x : 'a [@bits bits]
      ; in_valid : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { y : 'a [@bits bits]
      ; out_valid : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end
end

module With_interface_multiply_by_constant (M : Width) = struct
  include Interface_1arg (M)

  let create ~config ~multiply_by scope { I.clock; enable; x; in_valid } =
    let spec = Reg_spec.create ~clock () in
    let y =
      Multiply (x, Signal.of_constant (Bits.to_constant multiply_by))
      |> create_with_config ~scope ~clock ~enable ~config
    in
    let out_valid = pipeline spec ~enable ~n:(Config.latency config) in_valid in
    assert (width y = bits);
    { O.y; out_valid }
  ;;

  let hierarchical ~name ~config ~multiply_by scope i =
    let module H = Hierarchy.In_scope (I) (O) in
    let { O.y; out_valid = _ } =
      H.hierarchical ~scope ~name (create ~multiply_by ~config) i
    in
    y
  ;;
end

module With_interface_square (M : Width) = struct
  include Interface_1arg (M)

  let create
      ~config:{ Config.levels; ground_multiplier }
      scope
      { I.clock; enable; x; in_valid }
    =
    let spec = Reg_spec.create ~clock () in
    let y =
      create_recursive ~scope ~clock ~enable ~levels ~ground_multiplier (Square x)
    in
    let out_valid =
      pipeline spec ~enable ~n:(Config.latency { levels; ground_multiplier }) in_valid
    in
    assert (width y = bits);
    { O.y; out_valid }
  ;;

  let hierarchical ~name ~config scope i =
    let module H = Hierarchy.In_scope (I) (O) in
    let { O.y; out_valid = _ } = H.hierarchical ~scope ~name (create ~config) i in
    y
  ;;
end

let hierarchical ?name ~config ~clock ~enable ~scope (input : Multiplier_input.t) =
  let bits = Multiplier_input.width input in
  let module Width = struct
    let bits = bits
  end
  in
  match input with
  | Multiply (x, y) ->
    let module M = With_interface_multiply (Width) in
    let name =
      Option.value ~default:(Printf.sprintf "half_width_multiply_%d" bits) name
    in
    M.hierarchical ~name ~config scope { clock; enable; x; y; in_valid = vdd }
  | Multiply_by_constant (x, multiply_by) ->
    let module M = With_interface_multiply_by_constant (Width) in
    let name =
      Option.value ~default:(Printf.sprintf "half_width_multiply_by_cst_%d" bits) name
    in
    M.hierarchical ~name ~config ~multiply_by scope { clock; enable; x; in_valid = vdd }
  | Square x ->
    let module M = With_interface_square (Width) in
    let name = Option.value ~default:(Printf.sprintf "half_width_square_%d" bits) name in
    M.hierarchical ~name ~config scope { clock; enable; x; in_valid = vdd }
;;
