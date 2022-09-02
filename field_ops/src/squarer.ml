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

let rec create_recursive ~scope ~clock ~enable ~ground_multiplier ~levels (x : Signal.t) =
  match levels with
  | [] -> Ground_multiplier.create ~clock ~enable ~config:ground_multiplier x x
  | this_level :: levels ->
    create_level ~scope ~clock ~enable ~ground_multiplier ~this_level ~levels x

and create_level
    ~scope
    ~clock
    ~enable
    ~ground_multiplier
    ~this_level:
      { Config.Level.radix
      ; pre_adder_stages = _
      ; middle_adder_stages = _
      ; post_adder_stages
      }
    ~levels
    (x : Signal.t)
  =
  let spec = Reg_spec.create ~clock () in
  let pipeline ~n x = if is_const x then x else pipeline ~enable spec x ~n in
  let child_karatsuba_config =
    Karatsuba_ofman_mult.Config.generate ~ground_multiplier levels
  in
  let create_recursive input =
    let child_karatsuba_latency =
      Karatsuba_ofman_mult.Config.latency child_karatsuba_config
    in
    let child_squarer_latency = Config.latency { ground_multiplier; levels } in
    if child_karatsuba_latency < child_squarer_latency
    then
      raise_s
        [%message
          "child squarer latency should always be less than child karatsuba latency!"
            (child_squarer_latency : int)
            (child_karatsuba_latency : int)
            (levels : Config.Level.t list)
            (ground_multiplier : Ground_multiplier.Config.t)];
    create_recursive ~scope ~clock ~enable ~ground_multiplier ~levels input
    |> pipeline ~n:(child_karatsuba_latency - child_squarer_latency)
  in
  let create_full_multiplier a b =
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
  let w = width x in
  match radix with
  | Radix.Radix_2 ->
    let hw = (w + 1) / 2 in
    let top_and_btm_half x = uresize (drop_bottom x hw) hw, sel_bottom x hw in
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
    let x1, x0 = top_and_btm_half x in
    let x1_times_x0 = uresize (create_full_multiplier x1 x0) (2 * w) in
    let x1_times_x1 = uresize (create_recursive x1) (2 * w) in
    let x0_times_x0 = uresize (create_recursive x0) (2 * w) in
    pipeline
      ~n:post_adder_stages
      (sll x1_times_x1 (2 * hw) +: sll x1_times_x0 (hw + 1) +: x0_times_x0)
  | Radix.Radix_3 ->
    (* ((x2 * 2^(2k)) + (x1 * 2^k) + x0)^2
     *  = x2^2 * 2^4k
     *    + x2x1 * 2^3k
     *    + x2x0 * 2^2k
     *    + x1x2 * 2^3k
     *    + x1^2 * 2^2k
     *    + x1x0 * 2^k
     *    + x0x2 * 2^2k
     *    + x0x1 * 2^k
     *    + x0^2
     *  = x2^2 * 2^4k
     *    + (2 * x2x1) * 2^3k
     *    + (x1^2 + 2 * x2x0) * 2^2k
     *    + (2 * x0x1) * 2^k
     *    + x0^2
     *)
    let k = Int.round_up ~to_multiple_of:3 w / 3 in
    let x2, x1, x0 =
      match split_msb ~exact:true ~part_width:k (uresize x (3 * k)) with
      | [ a; b; c ] -> a, b, c
      | _ -> assert false
    in
    let x2_squared = uresize (create_recursive x2) (2 * w) in
    let x1_squared = uresize (create_recursive x1) (2 * w) in
    let x0_squared = uresize (create_recursive x0) (2 * w) in
    let x2x1 = uresize (create_full_multiplier x2 x1) (2 * w) in
    let x2x0 = uresize (create_full_multiplier x2 x0) (2 * w) in
    let x1x0 = uresize (create_full_multiplier x1 x0) (2 * w) in
    pipeline
      ~n:post_adder_stages
      (sll x2_squared (4 * k)
      +: sll x2x1 ((3 * k) + 1)
      +: sll x2x0 ((2 * k) + 1)
      +: sll x1_squared (2 * k)
      +: sll x1x0 (k + 1)
      +: x0_squared)
;;

module type Width = sig
  val bits : int
end

module With_interface (M : Width) = struct
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
      { y : 'a [@bits 2 * bits]
      ; out_valid : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create ~config scope { I.clock; enable; x; in_valid } =
    let { Config.levels; ground_multiplier } = config in
    let spec = Reg_spec.create ~clock () in
    let y = create_recursive ~scope ~clock ~enable ~levels ~ground_multiplier x in
    let out_valid = pipeline spec ~enable ~n:(Config.latency config) in_valid in
    assert (width y = 2 * bits);
    { O.y; out_valid }
  ;;

  let hierarchical ~name ~config scope i =
    let module H = Hierarchy.In_scope (I) (O) in
    let { O.y; out_valid = _ } = H.hierarchical ~scope ~name (create ~config) i in
    y
  ;;
end

let create_width_arg bits : (module Width) =
  (module struct
    let bits = bits
  end)
;;

let hierarchical ?name ~config ~clock ~enable ~scope (x : Signal.t) =
  let bits = Signal.width x in
  let module M = With_interface ((val create_width_arg bits)) in
  let name = Option.value ~default:(Printf.sprintf "square_%d" bits) name in
  M.hierarchical ~name ~config scope { clock; enable; x; in_valid = vdd }
;;
