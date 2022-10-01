open Core
open Hardcaml
open Signal

include struct
  open Field_ops_lib
  module Arbitrate = Arbitrate
  module Adder_subtractor_pipe = Adder_subtractor_pipe
  module Modulo_adder_pipe = Modulo_adder_pipe
  module Modulo_subtractor_pipe = Modulo_subtractor_pipe
  module Bram_reduce = Bram_reduce
end

include struct
  open Elliptic_curve_lib
  module Config_presets = Config_presets
  module Ec_fpn_ops_config = Ec_fpn_ops_config
end

module Model = Twisted_edwards_model_lib
module Modulo_ops = Model.Bls12_377_util.Modulo_ops

module Make (Num_bits : Num_bits.S) = struct
  open Num_bits
  module Xyt = Xyt.Make (Num_bits)
  module Xyzt = Xyzt.Make (Num_bits)

  module I = struct
    type 'a t =
      { clock : 'a
      ; valid_in : 'a
      ; p1 : 'a Xyzt.t [@rtlprefix "p1$"]
      ; p2 : 'a Xyt.t [@rtlprefix "p2$"]
      ; subtract : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { valid_out : 'a
      ; p3 : 'a Xyzt.t [@rtlprefix "p3$"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let multiply ~(config : Config.t) ~scope ~clock ~latency ~include_fine_reduction (x, y) =
    let enable = vdd in
    assert (width x = width y);
    let scope = Scope.sub_scope scope "multiply" in
    let reduce = if include_fine_reduction then Config.reduce else Config.coarse_reduce in
    let final_pipelining =
      latency config
      - Config.multiply_latency
          ~reduce:(if include_fine_reduction then Fine else Coarse)
          config
    in
    assert (final_pipelining >= 0);
    config.multiply.impl ~scope ~clock ~enable x (Some y)
    |> reduce config ~scope ~clock ~enable
    |> pipeline (Reg_spec.create ~clock ()) ~enable ~n:final_pipelining
  ;;

  let double_multiply
    ~include_fine_reduction
    ~(config : Config.t)
    ~scope
    ~clock
    ~valid
    ~latency
    ~latency_without_arbitration
    (x1, y1)
    (x2, y2)
    =
    let enable = vdd in
    assert (width y1 = width y2);
    assert (width x1 = width x2);
    let wy = width y1 in
    let wx = width x1 in
    let multiply ?(scope = scope) ~latency (a, b) =
      multiply ~scope ~config ~clock ~latency ~include_fine_reduction (a, b)
    in
    if config.arbitrated_multiplier
    then (
      let scope = Scope.sub_scope scope "arbed_multiply" in
      Arbitrate.arbitrate2
        (x1 @: y1, x2 @: y2)
        ~enable
        ~clock
        ~valid
        ~f:(fun input ->
          let y = sel_bottom input wy in
          let x = sel_top input wx in
          multiply (x, y) ~scope ~latency:latency_without_arbitration))
    else multiply ~latency (x1, y1), multiply ~latency (x2, y2)
  ;;

  let concat_result { Adder_subtractor_pipe.O.carry; result } = carry @: result

  let add_pipe ~scope ~latency ~(config : Config.t) ~clock a b =
    assert (width a = width b);
    let spec = Reg_spec.create ~clock () in
    let stages = config.adder_stages in
    let enable = vdd in
    let res =
      Adder_subtractor_pipe.add ~scope ~clock ~enable ~stages [ a; b ]
      |> concat_result
      |> fun x ->
      let n = latency config - Adder_subtractor_pipe.latency ~stages in
      assert (n >= 0);
      if n = 0 then x else pipeline spec ~n ~enable x
    in
    [%test_result: int] (width res) ~expect:(width a + 1);
    (*print_s [%message "add_pipe" (width res : int) (width a : int)];*)
    res
  ;;

  let sub_pipe ~scope ~latency ~(config : Config.t) ~clock a b =
    assert (width a = width b);
    let enable = vdd in
    let width = width a in
    let spec = Reg_spec.create ~clock () in
    let stages = config.subtractor_stages in
    let extra_bits = width - num_bits in
    Adder_subtractor_pipe.mixed
      ~scope
      ~clock
      ~enable
      ~stages
      ~init:a
      [ Sub b; Add (Signal.of_z Z.(config.p lsl extra_bits) ~width) ]
    |> concat_result
    |> fun x ->
    let n = latency config - Adder_subtractor_pipe.latency ~stages in
    assert (n >= 0);
    if n = 0 then x else pipeline spec ~n ~enable x
  ;;

  module Reduce_stage (I : sig
    include Interface.S

    val error : int
    val accumulated_error : int
  end) =
  struct
    let accumulated_error = I.(error + accumulated_error)

    module Bram_reduce_of_config (C : sig
      val config : Config.t
    end) =
    struct
      open C

      module Bram_config = struct
        let p = config.p
        let adder_stages = config.adder_stages
        let num_bits = num_bits
        let error_bits = accumulated_error
      end

      module Bram_reduce = Bram_reduce.Make (Bram_config)
    end

    let latency (config : Config.t) =
      let module B =
        Bram_reduce_of_config (struct
          let config = config
        end)
      in
      B.Bram_reduce.latency
    ;;

    let create ~build_mode ~(config : Config.t) ~scope ~clock (i : _ I.t) =
      let scope = Scope.sub_scope scope "reduce_stage" in
      let module B =
        Bram_reduce_of_config (struct
          let config = config
        end)
      in
      (* wrap the bram reduction *)
      let reduce coarse_value =
        let reduce =
          B.Bram_reduce.(hierarchical ~build_mode scope { I.clock; coarse_value })
        in
        reduce.reduced_value
      in
      let pipe =
        let spec = Reg_spec.create ~clock () in
        pipeline spec ~n:(latency config)
      in
      I.(
        map2 i port_names ~f:(fun v name ->
          (* CR rahuly : pretty hacky; should use a [With_valid] but the rest of the file doesn't *)
          (if String.(name = "valid")
          then pipe v
          else (
            [%test_result: int] (width v) ~expect:B.Bram_config.(num_bits + error_bits);
            reduce v))
          |> fun x -> Scope.naming scope x name))
    ;;
  end

  module Datapath_input = struct
    type 'a t =
      { p1 : 'a Xyzt.t
      ; p2 : 'a Xyt.t
      ; subtract : 'a
      ; valid : 'a
      }
  end

  module Stage0 = struct
    type 'a t =
      { c_A : 'a
      ; c_B : 'a
      ; c_C : 'a
      ; c_D : 'a
      ; subtract : 'a
      ; valid : 'a
      }
    [@@deriving sexp_of, hardcaml]

    let include_fine_reduction = false

    (* CR rahul: need to get this correctly from the config - it's the log2 of error introduced by 
     * the msb approximation *)
    let error = if include_fine_reduction then 0 else 4

    let latency_without_arbitration (config : Config.t) =
      Config.multiply_latency
        ~reduce:(if include_fine_reduction then Fine else Coarse)
        config
    ;;

    let latency (config : Config.t) =
      latency_without_arbitration config + if config.arbitrated_multiplier then 1 else 0
    ;;

    let create ~config ~scope ~clock { Datapath_input.p1; p2; subtract; valid } =
      [%test_result: int] (width p1.x) ~expect:num_bits;
      [%test_result: int] (width p1.y) ~expect:num_bits;
      [%test_result: int] (width p2.x) ~expect:num_bits;
      [%test_result: int] (width p2.y) ~expect:num_bits;
      let spec = Reg_spec.create ~clock () in
      let pipe = pipeline spec ~n:(latency config) in
      let c_A, c_B =
        double_multiply
          ~include_fine_reduction
          ~config
          ~scope
          ~clock
          ~valid
          ~latency
          ~latency_without_arbitration
          (p1.x, p2.x)
          (p1.y, p2.y)
      in
      let c_C =
        multiply ~include_fine_reduction ~latency ~config ~scope ~clock (p1.t, p2.t)
      in
      let scope = Scope.sub_scope scope "stage1" in
      { c_A; c_B; c_C; c_D = pipe p1.z; subtract = pipe subtract; valid = pipe valid }
      |> map2 port_names ~f:(fun name x -> Scope.naming scope x name)
    ;;
  end

  module Stage1 = struct
    type 'a t =
      { c_E : 'a
      ; c_F : 'a
      ; c_G : 'a
      ; c_H : 'a
      ; valid : 'a
      }
    [@@deriving sexp_of, hardcaml]

    let accumulated_error = Stage0.error
    let error = 1
    let latency (config : Config.t) = config.adder_stages

    let create ~config ~scope ~clock { Stage0.c_A; c_B; c_C; c_D; subtract; valid } =
      [%test_result: int] (width c_A) ~expect:(num_bits + accumulated_error);
      [%test_result: int] (width c_B) ~expect:(num_bits + accumulated_error);
      [%test_result: int] (width c_C) ~expect:(num_bits + accumulated_error);
      [%test_result: int] (width c_D) ~expect:num_bits;
      let c_D = uresize c_D (num_bits + accumulated_error) in
      let spec = Reg_spec.create ~clock () in
      let pipe = pipeline spec ~n:(latency config) in
      (* Consider arb-ing here? *)
      let re s = uresize s (num_bits + accumulated_error + error) in
      let c_E = sub_pipe ~scope ~latency ~config ~clock c_B c_A |> re in
      let c_D_minus_c_C = sub_pipe ~scope ~latency ~config ~clock c_D c_C |> re in
      let c_D_plus_c_C = add_pipe ~scope ~latency ~config ~clock c_D c_C |> re in
      let c_H = add_pipe ~scope ~latency ~config ~clock c_B c_A |> re in
      (* assign based on the sign of t2 *)
      let subtract = pipe subtract in
      let c_F = mux2 subtract c_D_plus_c_C c_D_minus_c_C in
      let c_G = mux2 subtract c_D_minus_c_C c_D_plus_c_C in
      let scope = Scope.sub_scope scope "stage2" in
      { c_E; c_F; c_G; c_H; valid = pipe valid }
      |> map2 port_names ~f:(fun name x -> Scope.naming scope x name)
    ;;
  end

  module Stage2 = Reduce_stage (Stage1)

  module Stage3 = struct
    type 'a t =
      { pre_x3 : 'a
      ; pre_y3 : 'a
      ; z3 : 'a
      ; t3 : 'a
      ; valid : 'a
      }
    [@@deriving sexp_of, hardcaml]

    let include_fine_reduction = false

    (* CR rahul: need to get this correctly from the config - it's the log2 of error introduced by 
     * the msb approximation *)
    let error = if include_fine_reduction then 0 else 4

    let latency_without_arbitration (config : Config.t) =
      Config.multiply_latency
        ~reduce:(if include_fine_reduction then Fine else Coarse)
        config
    ;;

    let latency (config : Config.t) =
      latency_without_arbitration config + if config.arbitrated_multiplier then 1 else 0
    ;;

    let create ~config ~scope ~clock { Stage1.c_E; c_F; c_G; c_H; valid } =
      let spec_with_clear = Reg_spec.create ~clock () in
      let pipe_with_clear = pipeline spec_with_clear ~n:(latency config) in
      let pre_x3, pre_y3 =
        double_multiply
          ~config
          ~scope
          ~clock
          ~valid
          ~latency
          ~latency_without_arbitration
          ~include_fine_reduction
          (c_E, c_F)
          (c_G, c_H)
      in
      let t3, z3 =
        double_multiply
          ~config
          ~scope
          ~clock
          ~valid
          ~latency
          ~latency_without_arbitration
          ~include_fine_reduction
          (c_E, c_H)
          (c_F, c_G)
      in
      let scope = Scope.sub_scope scope "stage3" in
      { pre_x3; pre_y3; z3; t3; valid = pipe_with_clear valid }
      |> map2 port_names ~f:(fun name x -> Scope.naming scope x name)
    ;;
  end

  module Stage4 = struct
    let accumulated_error = Stage3.error
    let error = 1

    type 'a t =
      { x3 : 'a
      ; y3 : 'a
      ; z3 : 'a
      ; t3 : 'a
      ; valid : 'a
      }
    [@@deriving sexp_of, hardcaml]

    let latency (config : Config.t) = config.adder_stages

    let create ~config ~scope ~clock { Stage3.pre_x3; pre_y3; z3; t3; valid } =
      [%test_result: int] (width pre_x3) ~expect:(num_bits + accumulated_error);
      [%test_result: int] (width pre_y3) ~expect:(num_bits + accumulated_error);
      [%test_result: int] (width z3) ~expect:(num_bits + accumulated_error);
      [%test_result: int] (width t3) ~expect:(num_bits + accumulated_error);
      let spec = Reg_spec.create ~clock () in
      let pipe = pipeline spec ~n:(latency config) in
      assert (config.adder_stages = config.subtractor_stages);
      let re s = uresize s (num_bits + accumulated_error + error) in
      let x3 = sub_pipe ~scope ~latency ~config ~clock pre_y3 pre_x3 |> re in
      let y3 = add_pipe ~scope ~latency ~config ~clock pre_y3 pre_x3 |> re in
      let z3 = pipe z3 |> re in
      let t3 = pipe t3 |> re in
      let scope = Scope.sub_scope scope "stage4" in
      { x3; y3; z3; t3; valid = pipe valid }
      |> map2 port_names ~f:(fun name x -> Scope.naming scope x name)
    ;;
  end

  module Stage5 = Reduce_stage (Stage4)

  let output_pipes = 2

  let latency config =
    Stage0.latency config
    + Stage1.latency config
    + Stage2.latency config
    + Stage3.latency config
    + Stage4.latency config
    + Stage5.latency config
    + output_pipes
  ;;

  let create
    ?(build_mode = Build_mode.Simulation)
    ~config
    scope
    { I.clock; valid_in; p1; p2; subtract }
    =
    (* when we subtract, we want to output [p1-p2] instead of [p1+p2].
     * Because p2 is coming from the host, we use
     *    - [1/2 * (y - x)] in place of x
     *    - [1/2 * (y + x)] in place of y
     *    - [4d  * (t    )] in place of t
     * So, because negating a point in twisted edwards is equivalent to negating the x coordinate, 
     * we have to swap x,y and negate t. We push the t negation down the computation to avoid 
     * adding another subtractor. *)
    let p2 = Xyt.Of_signal.mux2 subtract { Xyt.x = p2.y; y = p2.x; t = p2.t } p2 in
    let { Stage4.x3; y3; z3; t3; valid = valid_out } =
      { p1; p2; subtract; valid = valid_in }
      |> Stage0.create ~config ~scope ~clock
      |> Stage1.create ~config ~scope ~clock
      |> Stage2.create ~build_mode ~config ~scope ~clock
      |> Stage3.create ~config ~scope ~clock
      |> Stage4.create ~config ~scope ~clock
      |> Stage5.create ~build_mode ~config ~scope ~clock
      |> Stage4.Of_signal.pipeline ~n:output_pipes (Reg_spec.create ~clock ())
    in
    { O.valid_out; p3 = { x = x3; y = y3; z = z3; t = t3 } }
  ;;

  let hierarchical ?build_mode ?instance ~config scope =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ?instance ~name:"adder_precompute" ~scope (create ?build_mode ~config)
  ;;
end
