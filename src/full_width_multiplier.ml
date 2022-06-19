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

let rec create_recursive ~scope ~clock ~enable ~(config : Config.t) a =
  let spec = Reg_spec.create ~clock () in
  let pipeline ~n x = if is_const x then x else pipeline ~enable spec x ~n in
  match config.depth with
  | 0 -> Ground_multiplier.create ~clock ~enable ~config:config.ground_multiplier a a
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
    let recurse input =
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
    let w = width a in
    let hw = (w + 1) / 2 in
    let t = uresize (drop_bottom a hw) hw in
    let b = sel_bottom a hw in
    (* ((t * 2^hw) + b) * ((t * 2^hw) + b)
     * = (t^2 * 2^(2 * hw)) + (t * b * 2^(1 + hw)) + b^2
     *)
    let top_squared = recurse t in
    let btm_squared = recurse b in
    let top_times_bottom = create_full_multiplier t b in
    assert (width top_squared = hw * 2);
    assert (width btm_squared = hw * 2);
    assert (width top_times_bottom = hw * 2);
    let top_times_bottom_shifted = top_times_bottom @: zero (hw + 1) in
    let top_squared_shifted = top_squared @: zero (2 * hw) in
    pipeline
      ~n:post_adder_stages
      (uresize top_squared_shifted (2 * w)
      +: uresize Uop.(top_times_bottom_shifted +: btm_squared) (2 * w))
;;

let create = create_recursive
