open Base
open Hardcaml
open Signal
open Reg_with_enable

module Config = struct
  module Level = struct
    type t =
      { k : int -> int
      ; for_karatsuba : Karatsuba_ofman_mult.Config.Level.t
      }
  end

  type t =
    { levels : Level.t list
    ; ground_multiplier : Ground_multiplier.Config.t
    }

  let latency { levels; ground_multiplier } =
    match levels with
    | [] -> Ground_multiplier.Config.latency ground_multiplier
    | { for_karatsuba =
          { radix = _; pre_adder_stages = _; middle_adder_stages = _; post_adder_stages }
      ; k = _
      }
      :: tl ->
      let slowest_multiplier =
        List.map tl ~f:(fun x -> x.for_karatsuba)
        |> Karatsuba_ofman_mult.Config.generate ~ground_multiplier
        |> Karatsuba_ofman_mult.Config.latency
      in
      slowest_multiplier + post_adder_stages
  ;;
end

module Input = Multiplier_input

let split2 ~k x = drop_bottom x k, sel_bottom x k
let split3 ~k x = drop_bottom x (2 * k), sel_bottom (drop_bottom x k) k, sel_bottom x k

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
    With_shift.no_shift
      (Ground_multiplier.create ~clock ~enable ~config:ground_multiplier a b)
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
    { Config.Level.for_karatsuba =
        { radix; middle_adder_stages = _; pre_adder_stages = _; post_adder_stages }
    ; k = calc_k
    }
  (input : Input.t)
  =
  let spec = Reg_spec.create ~clock () in
  let pipeline ~n x = if is_const x then x else pipeline ~enable spec x ~n in
  let child_karatsuba_config =
    Karatsuba_ofman_mult.Config.generate
      ~ground_multiplier
      (List.map remaining_levels ~f:(fun l -> l.for_karatsuba))
  in
  let create_recursive input =
    let n =
      Karatsuba_ofman_mult.Config.latency child_karatsuba_config
      - Config.latency { levels = remaining_levels; ground_multiplier }
    in
    create_recursive
      ~scope
      ~clock
      ~enable
      ~levels:remaining_levels
      ~ground_multiplier
      input
    |> With_shift.map ~f:(pipeline ~n)
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
  let w = Multiplier_input.width input in
  let w2 = w * 2 in
  let k = calc_k w in
  let pipe_add ~stages items = With_shift.pipe_add ~scope ~enable ~clock ~stages items in
  match radix with
  | Radix.Radix_2 ->
    (* ((ta * 2^hw) + ba) * ((tb * 2^hw) + bb)
     * = ((ua * ub) * 2^(2 * hw)) + (((ua * lb) + (ub * la)) * 2^hw) + (la * lb)
     *
     * The upper-halve is zero in a part-width hader, reduces to the following
     * approximation
     *
     * ((ua * ub) * 2^(2 * hw)) + (((ua * lb) + (ub * la)) * 2^hw)
     *
     * See approx_msb_multiplier_model.ml for information about what the
     * maximum error this can produce.
     *)
    (match input with
     | Multiply_by_constant _ -> assert false
     | Multiply (a, b) ->
       let ua, la = split2 ~k a in
       let ub, lb = split2 ~k b in
       let wu = width ua in
       let ua_mult_lb = create_recursive (Multiply (ua, uresize lb wu)) in
       let ub_mult_la = create_recursive (Multiply (ub, uresize la wu)) in
       let ua_mult_ub = create_full_multiplier ua ub in
       let result =
         pipe_add
           ~stages:post_adder_stages
           [ With_shift.uresize ua_mult_lb (w2 - k)
           ; With_shift.uresize ub_mult_la (w2 - k)
           ; With_shift.uresize (With_shift.create ~shift:k ua_mult_ub) (w2 - k)
           ]
         |> With_shift.sll ~by:k
       in
       assert (With_shift.width result = w * 2);
       result
     | Square _ -> raise_s [%message "Approx MSB squaring specialization not implemented"])
  | Radix_3 ->
    (match input with
     | Multiply_by_constant _ -> assert false
     | Multiply (x, y) ->
       (* See comments in approx_msb_multiplier_model.ml for why this works. It's
        * similar to the idea in half_width_multiplier.ml
        *)
       let k2 = k * 2 in
       let k3 = k * 3 in
       let k4 = k * 4 in
       let x2, x1, x0 = split3 ~k x in
       let y2, y1, y0 = split3 ~k y in
       let wx2 = width x2 in
       assert (width x1 = k);
       assert (width x0 = k);
       assert (width y2 = wx2);
       assert (width y1 = k);
       assert (width y0 = k);
       assert (wx2 >= k);
       assert (wx2 + k + k = w);
       let x2y2 = create_full_multiplier (uresize x2 wx2) (uresize y2 wx2) in
       let x2y1 = create_full_multiplier (uresize x2 wx2) (uresize y1 wx2) in
       let x1y2 = create_full_multiplier (uresize x1 wx2) (uresize y2 wx2) in
       let x2y0 = create_recursive (Multiply (uresize x2 wx2, uresize y0 wx2)) in
       let x1y1 =
         (* CR-someday fyquah: The upcast to wx2 is strictly not necessary.
          * But we're keeping it for now due to empirical better DSP usage (at
          * the cost of some LUTs). This is likely due to vivado offloading some
          * multiplications to LUTs when there are more zeros (due to the
          * uresize). Revisit this when we implement custom logic to implement
          * constant multiplication specially.
          *)
         create_recursive (Multiply (uresize x1 wx2, uresize y1 wx2))
       in
       let x0y2 = create_recursive (Multiply (uresize x0 wx2, uresize y2 wx2)) in
       let result =
         pipe_add
           ~stages:post_adder_stages
           [ With_shift.create x2y2 ~shift:(k4 - k2)
           ; With_shift.uresize (With_shift.create x2y1 ~shift:(k3 - k2)) (w2 - k2)
           ; With_shift.uresize (With_shift.create x1y2 ~shift:(k3 - k2)) (w2 - k2)
           ; With_shift.uresize x2y0 ((w * 2) - k2)
           ; With_shift.uresize x1y1 ((w * 2) - k2)
           ; With_shift.uresize x0y2 ((w * 2) - k2)
           ]
       in
       With_shift.sll result ~by:k2
     | Square _ ->
       raise_s [%message "Radix-3 not implemented in approx msb multiplication."])
;;

let create_with_config
  ~config:{ Config.levels; ground_multiplier }
  ~scope
  ~clock
  ~enable
  input
  =
  create_recursive ~levels ~ground_multiplier ~scope ~clock ~enable input
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
      { z : 'a [@bits 2 * bits]
      ; out_valid : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create ~config scope { I.clock; enable; x; y; in_valid } =
    let spec = Reg_spec.create ~clock () in
    let z =
      create_with_config ~config ~scope ~clock ~enable (Multiply (x, y))
      |> With_shift.to_signal
    in
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
      { y : 'a [@bits 2 * bits]
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
      |> With_shift.to_signal
    in
    let out_valid = pipeline spec ~enable ~n:(Config.latency config) in_valid in
    assert (width y = bits * 2);
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
      |> With_shift.to_signal
    in
    let out_valid =
      pipeline spec ~enable ~n:(Config.latency { levels; ground_multiplier }) in_valid
    in
    assert (width y = 2 * bits);
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
      Option.value ~default:(Printf.sprintf "approx_msb_multiply_%d" bits) name
    in
    M.hierarchical ~name ~config scope { clock; enable; x; y; in_valid = vdd }
  | Multiply_by_constant (x, multiply_by) ->
    let module M = With_interface_multiply_by_constant (Width) in
    let name =
      Option.value ~default:(Printf.sprintf "approx_msb_multiply_by_cst_%d" bits) name
    in
    M.hierarchical ~name ~config ~multiply_by scope { clock; enable; x; in_valid = vdd }
  | Square x ->
    let module M = With_interface_square (Width) in
    let name =
      Option.value ~default:(Printf.sprintf "approx_msb_width_square_%d" bits) name
    in
    M.hierarchical ~name ~config scope { clock; enable; x; in_valid = vdd }
;;
