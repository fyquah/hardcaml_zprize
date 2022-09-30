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

  let arbitrate_multiply
    ~include_fine_reduction
    ~(config : Config.t)
    ~scope
    ~clock
    ~valid
    ~latency_without_arbitration
    (x1, y1)
    (x2, y2)
    =
    let enable = vdd in
    assert (width y1 = width y2);
    assert (width x1 = width x2);
    let wy = width y1 in
    let wx = width x1 in
    let scope = Scope.sub_scope scope "arbed_multiply" in
    Arbitrate.arbitrate2
      (x1 @: y1, x2 @: y2)
      ~enable
      ~clock
      ~valid
      ~f:(fun input ->
        let y = sel_bottom input wy in
        let x = sel_top input wx in
        multiply
          ~config
          ~scope
          ~clock
          ~latency:latency_without_arbitration
          ~include_fine_reduction
          (x, y))
  ;;

  let concat_result { Adder_subtractor_pipe.O.carry; result } = carry @: result

  let mod_add_pipe ~scope ~latency ~(config : Config.t) ~clock a b =
    let spec = Reg_spec.create ~clock () in
    let stages = config.adder_stages in
    Modulo_adder_pipe.hierarchical
      ~scope
      ~clock
      ~enable:vdd
      ~stages:config.adder_stages
      ~p:config.p
      a
      b
    |> fun x ->
    let n = latency config - Modulo_adder_pipe.latency ~stages in
    assert (n >= 0);
    if n = 0 then x else pipeline spec ~n ~enable:vdd x
  ;;

  let mod_sub_pipe ~scope:_ ~latency ~(config : Config.t) ~clock a b =
    let spec = Reg_spec.create ~clock () in
    let stages = config.subtractor_stages in
    Modulo_subtractor_pipe.create ~clock ~enable:vdd ~stages ~p:config.p a b
    |> fun x ->
    let n = latency config - Modulo_adder_pipe.latency ~stages in
    assert (n >= 0);
    if n = 0 then x else pipeline spec ~n ~enable:vdd x
  ;;

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

  let _ = mod_add_pipe
  let _ = mod_sub_pipe
  let _ = add_pipe
  let _ = sub_pipe

  module Datapath_input = struct
    type 'a t =
      { p1 : 'a Xyzt.t
      ; p2 : 'a Xyt.t
      ; valid : 'a
      }
  end

  module Stage0 = struct
    let error = 0

    type 'a t =
      { p1 : 'a Xyzt.t [@rtlprefix "p1$"]
      ; p2 : 'a Xyt.t [@rtlprefix "p2$"]
      ; y1_plus_x1 : 'a
      ; y1_minus_x1 : 'a
      ; valid : 'a
      }
    [@@deriving sexp_of, hardcaml]

    let latency (config : Config.t) = config.adder_stages

    let create ~config ~scope ~clock { Datapath_input.p1; p2; valid } =
      let spec = Reg_spec.create ~clock () in
      let pipe = pipeline spec ~n:(latency config) in
      assert (config.adder_stages = config.subtractor_stages);
      let y1_plus_x1 =
        mod_add_pipe
          ~scope
          ~latency:(Fn.const config.adder_stages)
          ~config
          ~clock
          p1.y
          p1.x
      in
      let y1_minus_x1 =
        mod_sub_pipe
          ~scope
          ~latency:(Fn.const config.adder_stages)
          ~config
          ~clock
          p1.y
          p1.x
        |> Fn.flip sel_bottom (num_bits + error)
      in
      [%test_result: int] (width y1_plus_x1) ~expect:(num_bits + error);
      [%test_result: int] (width y1_minus_x1) ~expect:(num_bits + error);
      let scope = Scope.sub_scope scope "stage0" in
      { y1_plus_x1
      ; y1_minus_x1
      ; p1 = Xyzt.map ~f:pipe p1
      ; p2 = Xyt.map ~f:pipe p2
      ; valid = pipe valid
      }
      |> map2 port_names ~f:(fun name x -> Scope.naming scope x name)
    ;;
  end

  module Stage1 = struct
    type 'a t =
      { c_A : 'a
      ; c_B : 'a
      ; c_C : 'a
      ; c_D : 'a
      ; valid : 'a
      }
    [@@deriving sexp_of, hardcaml]

    let include_fine_reduction = false

    (* CR rahul: need to get this correctly from the config - it's the log2 of error introduced by 
     * the msb approximation *)
    let accumulated_error = Stage0.error
    let error = if include_fine_reduction then 0 else 4

    let latency_without_arbitration (config : Config.t) =
      Config.multiply_latency
        ~reduce:(if include_fine_reduction then Fine else Coarse)
        config
    ;;

    let latency (config : Config.t) =
      latency_without_arbitration config + if config.arbitrated_multiplier then 1 else 0
    ;;

    let create ~config ~scope ~clock { Stage0.p1; p2; y1_plus_x1; y1_minus_x1; valid } =
      [%test_result: int] (width y1_plus_x1) ~expect:(num_bits + accumulated_error);
      [%test_result: int] (width y1_minus_x1) ~expect:(num_bits + accumulated_error);
      [%test_result: int] (width p2.x) ~expect:num_bits;
      [%test_result: int] (width p2.y) ~expect:num_bits;
      let spec = Reg_spec.create ~clock () in
      let pipe = pipeline spec ~n:(latency config) in
      let x2 = uresize p2.x (num_bits + accumulated_error) in
      let y2 = uresize p2.y (num_bits + accumulated_error) in
      let c_A, c_B =
        if config.arbitrated_multiplier
        then
          arbitrate_multiply
            ~include_fine_reduction
            ~config
            ~scope
            ~clock
            ~valid
            ~latency_without_arbitration
            (y1_minus_x1, x2)
            (y1_plus_x1, y2)
        else
          ( multiply
              ~include_fine_reduction
              ~latency
              ~config
              ~scope
              ~clock
              (y1_minus_x1, x2)
          , multiply
              ~include_fine_reduction
              ~latency
              ~config
              ~scope
              ~clock
              (y1_plus_x1, y2) )
      in
      let c_C =
        multiply ~include_fine_reduction ~latency ~config ~scope ~clock (p1.t, p2.t)
      in
      let scope = Scope.sub_scope scope "stage1" in
      { c_A; c_B; c_C; c_D = pipe p1.z; valid = pipe valid }
      |> map2 port_names ~f:(fun name x -> Scope.naming scope x name)
    ;;
  end

  module Stage2 = struct
    type 'a t =
      { c_E : 'a
      ; c_F : 'a
      ; c_G : 'a
      ; c_H : 'a
      ; valid : 'a
      }
    [@@deriving sexp_of, hardcaml]

    let accumulated_error = Stage0.error + Stage1.error
    let error = 1
    let latency (config : Config.t) = config.adder_stages

    let create ~config ~scope ~clock { Stage1.c_A; c_B; c_C; c_D; valid } =
      [%test_result: int] (width c_A) ~expect:(num_bits + accumulated_error);
      [%test_result: int] (width c_B) ~expect:(num_bits + accumulated_error);
      [%test_result: int] (width c_C) ~expect:(num_bits + Stage1.error);
      [%test_result: int] (width c_D) ~expect:num_bits;
      let c_C = uresize c_C (num_bits + accumulated_error) in
      let c_D = uresize c_D (num_bits + accumulated_error) in
      let spec = Reg_spec.create ~clock () in
      let pipe = pipeline spec ~n:(latency config) in
      (* Consider arb-ing here? *)
      let re s =
        let target_width = num_bits + accumulated_error + error in
        (*print_s [%message "re" (width s : int) (target_width : int)];
        assert (width s <= target_width);*)
        uresize s target_width
      in
      let c_E = sub_pipe ~scope ~latency ~config ~clock c_B c_A |> re in
      let c_F = sub_pipe ~scope ~latency ~config ~clock c_D c_C |> re in
      let c_G = add_pipe ~scope ~latency ~config ~clock c_D c_C |> re in
      let c_H = add_pipe ~scope ~latency ~config ~clock c_B c_A |> re in
      let scope = Scope.sub_scope scope "stage2" in
      { c_E; c_F; c_G; c_H; valid = pipe valid }
      |> map2 port_names ~f:(fun name x -> Scope.naming scope x name)
    ;;
  end

  module Stage2_reduce = struct
    let accumulated_error = Stage0.error + Stage1.error + Stage2.error

    type 'a t =
      { c_E : 'a [@bits num_bits]
      ; c_F : 'a [@bits num_bits]
      ; c_G : 'a [@bits num_bits]
      ; c_H : 'a [@bits num_bits]
      ; valid : 'a
      }
    [@@deriving sexp_of, hardcaml]

    let latency (config : Config.t) =
      let module Bram_reduce =
        Bram_reduce.Make (struct
          let p = config.p
          let adder_stages = config.adder_stages
          let num_bits = num_bits
          let error_bits = accumulated_error
        end)
      in
      Bram_reduce.latency
    ;;

    let create
      ~build_mode
      ~(config : Config.t)
      ~scope
      ~clock
      { Stage2.c_E; c_F; c_G; c_H; valid }
      =
      (* create bram_reduce *)
      let module Bram_reduce =
        Bram_reduce.Make (struct
          let p = config.p
          let adder_stages = config.adder_stages
          let num_bits = num_bits
          let error_bits = accumulated_error
        end)
      in
      let reduce coarse_value =
        let reduce =
          Bram_reduce.(hierarchical ~build_mode scope { I.clock; coarse_value })
        in
        reduce.reduced_value
      in
      [%test_result: int] (width c_E) ~expect:(num_bits + accumulated_error);
      [%test_result: int] (width c_F) ~expect:(num_bits + accumulated_error);
      [%test_result: int] (width c_G) ~expect:(num_bits + accumulated_error);
      [%test_result: int] (width c_H) ~expect:(num_bits + accumulated_error);
      let spec = Reg_spec.create ~clock () in
      let pipe = pipeline spec ~n:(latency config) in
      (* Consider arb-ing here? *)
      let c_E = reduce c_E in
      let c_F = reduce c_F in
      let c_G = reduce c_G in
      let c_H = reduce c_H in
      let scope = Scope.sub_scope scope "stage2_reduce" in
      { c_E; c_F; c_G; c_H; valid = pipe valid }
      |> map2 port_names ~f:(fun name x -> Scope.naming scope x name)
    ;;
  end

  module Stage3 = struct
    type 'a t =
      { x3 : 'a
      ; y3 : 'a
      ; z3 : 'a
      ; t3 : 'a
      ; valid : 'a
      }
    [@@deriving sexp_of, hardcaml]

    let latency_without_arbitration (config : Config.t) =
      Config.multiply_latency ~reduce:Fine config
    ;;

    let latency (config : Config.t) =
      latency_without_arbitration config + if config.arbitrated_multiplier then 1 else 0
    ;;

    let create ~config ~scope ~clock { Stage2_reduce.c_E; c_F; c_G; c_H; valid } =
      let include_fine_reduction = true in
      let spec_with_clear = Reg_spec.create ~clock () in
      let pipe_with_clear = pipeline spec_with_clear ~n:(latency config) in
      let x3, y3 =
        if config.arbitrated_multiplier
        then
          arbitrate_multiply
            ~config
            ~scope
            ~clock
            ~valid
            ~latency_without_arbitration
            ~include_fine_reduction
            (c_E, c_F)
            (c_G, c_H)
        else
          ( multiply ~include_fine_reduction ~latency ~config ~scope ~clock (c_E, c_F)
          , multiply ~include_fine_reduction ~latency ~config ~scope ~clock (c_G, c_H) )
      in
      let t3, z3 =
        if config.arbitrated_multiplier
        then
          arbitrate_multiply
            ~config
            ~scope
            ~clock
            ~valid
            ~latency_without_arbitration
            ~include_fine_reduction
            (c_E, c_H)
            (c_F, c_G)
        else
          ( multiply ~include_fine_reduction ~latency ~config ~scope ~clock (c_E, c_H)
          , multiply ~include_fine_reduction ~latency ~config ~scope ~clock (c_F, c_G) )
      in
      let scope = Scope.sub_scope scope "stage3" in
      { x3; y3; z3; t3; valid = pipe_with_clear valid }
      |> map2 port_names ~f:(fun name x -> Scope.naming scope x name)
    ;;
  end

  let output_pipes = 2

  let latency config =
    Stage0.latency config
    + Stage1.latency config
    + Stage2.latency config
    + Stage2_reduce.latency config
    + Stage3.latency config
    + output_pipes
  ;;

  let create
    ?(build_mode = Build_mode.Simulation)
    ~config
    scope
    { I.clock; valid_in; p1; p2 }
    =
    let { Stage3.x3; y3; z3; t3; valid = valid_out } =
      { p1; p2; valid = valid_in }
      |> Stage0.create ~config ~scope ~clock
      |> Stage1.create ~config ~scope ~clock
      |> Stage2.create ~config ~scope ~clock
      |> Stage2_reduce.create ~build_mode ~config ~scope ~clock
      |> Stage3.create ~config ~scope ~clock
      |> Stage3.Of_signal.pipeline ~n:output_pipes (Reg_spec.create ~clock ())
    in
    { O.valid_out; p3 = { x = x3; y = y3; z = z3; t = t3 } }
  ;;

  let hierarchical ?build_mode ?instance ~config scope =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ?instance ~name:"adder_precompute" ~scope (create ?build_mode ~config)
  ;;
end
