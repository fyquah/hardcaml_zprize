open Base
open Hardcaml
open Signal
open Reg_with_enable

let post_adder_stages ~depth:_ = 1

module Config = struct
  type t =
    { depth : int
    ; ground_multiplier : Ground_multiplier.Config.t
    }

  let latency { depth; ground_multiplier } =
    match depth with
    | 0 -> Ground_multiplier.Config.latency ground_multiplier
    | _ ->
      let slowest_multiplier =
        List.init (depth - 1) ~f:(fun _ -> Radix.Radix_2)
        |> Karatsuba_ofman_mult.Config.generate ~ground_multiplier
        |> Karatsuba_ofman_mult.Config.latency
      in
      slowest_multiplier + post_adder_stages ~depth
  ;;
end

module Input = Multiplier_input

let rec create_recursive ~scope ~clock ~enable ~(config : Config.t) (x : Signal.t) =
  let spec = Reg_spec.create ~clock () in
  let pipeline ~n x = if is_const x then x else pipeline ~enable spec x ~n in
  match config.depth with
  | 0 -> Ground_multiplier.create ~clock ~enable ~config:config.ground_multiplier x x
  | _ ->
    let post_adder_stages = post_adder_stages ~depth:config.depth in
    let child_karatsuba_config =
      Karatsuba_ofman_mult.Config.generate
        ~ground_multiplier:config.ground_multiplier
        (List.init (config.depth - 1) ~f:(fun _ -> Radix.Radix_2))
    in
    let child_halfwidth_config = { config with depth = config.depth - 1 } in
    let create_recursive input =
      create_recursive ~scope ~clock ~enable ~config:child_halfwidth_config input
      |> pipeline
           ~n:
             (Karatsuba_ofman_mult.Config.latency child_karatsuba_config
             - Config.latency child_halfwidth_config)
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

  let create ~(config : Config.t) scope { I.clock; enable; x; in_valid } =
    let spec = Reg_spec.create ~clock () in
    let y = create_recursive ~scope ~clock ~enable ~config x in
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
