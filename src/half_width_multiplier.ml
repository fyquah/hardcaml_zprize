open Base
open Hardcaml
open Signal
open Reg_with_enable

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
        Karatsuba_ofman_mult.Config.generate ~ground_multiplier ~depth:(depth - 1)
        |> Karatsuba_ofman_mult.Config.latency
      in
      slowest_multiplier + Karatsuba_ofman_mult.Config.post_adder_stages ~depth
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
      | Multiply (a, b) -> a, b
      | Square a -> a, a
    in
    Ground_multiplier.create ~clock ~enable ~config:config.ground_multiplier a b
  | _ ->
    let post_adder_stages =
      Karatsuba_ofman_mult.Config.post_adder_stages ~depth:config.depth
    in
    let child_karatsuba_config =
      Karatsuba_ofman_mult.Config.generate
        ~ground_multiplier:config.ground_multiplier
        ~depth:(config.depth - 1)
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

module With_interface_multiply (M : sig
  val bits : int
end) =
struct
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
end
