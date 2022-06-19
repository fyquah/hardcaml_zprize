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

let rec create_recursive ~scope ~clock ~enable ~(config : Config.t) (input : Input.t) =
  let spec = Reg_spec.create ~clock () in
  let pipeline ~n x = if is_const x then x else pipeline ~enable spec x ~n in
  match config.depth with
  | 0 ->
    let a, b =
      match input with
      | Multiply_by_constant _ -> assert false
      | Multiply (a, b) -> a, b
      | Square a -> a, a
    in
    Ground_multiplier.create ~clock ~enable ~config:config.ground_multiplier a b
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
      let ua_mult_lb = sll (uresize (create_recursive (Multiply (ua, lb))) w) hw in
      let ub_mult_la = sll (uresize (create_recursive (Multiply (ub, la))) w) hw in
      let la_mult_lb = uresize (create_full_multiplier la lb) w in
      pipeline ~n:post_adder_stages (ua_mult_lb +: ub_mult_la +: la_mult_lb)
    | Square a ->
      let ua, la = top_and_btm_half a in
      let ua_mult_la_times_2 =
        sll (uresize (create_recursive (Multiply (ua, la))) w) (hw + 1)
      in
      let la_mult_la = uresize (create_full_multiplier la la) w in
      pipeline ~n:post_adder_stages (ua_mult_la_times_2 +: la_mult_la))
;;

let create = create_recursive

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

  let create ~(config : Config.t) scope { I.clock; enable; x; y; in_valid } =
    let spec = Reg_spec.create ~clock () in
    let z = create_recursive ~scope ~clock ~enable ~config (Multiply (x, y)) in
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

  let create ~(config : Config.t) ~multiply_by scope { I.clock; enable; x; in_valid } =
    let spec = Reg_spec.create ~clock () in
    let y =
      Multiply (x, Signal.of_constant (Bits.to_constant multiply_by))
      |> create_recursive ~scope ~clock ~enable ~config
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

  let create ~(config : Config.t) scope { I.clock; enable; x; in_valid } =
    let spec = Reg_spec.create ~clock () in
    let y = create_recursive ~scope ~clock ~enable ~config (Square x) in
    let out_valid = pipeline spec ~enable ~n:(Config.latency config) in_valid in
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
