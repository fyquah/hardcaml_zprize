open Core
open Hardcaml
open Signal

include struct
  open Field_ops_lib
  module Arbitrate = Arbitrate
  module Adder_subtractor_pipe = Adder_subtractor_pipe
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

  (* CR rahul: need to get this correctly from the config *)
  let num_correction_steps = 4

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

  let add_pipe ~scope ~latency ~(config : Config.t) ~clock a b =
    assert (width a = width b);
    let spec = Reg_spec.create ~clock () in
    let stages = config.adder_stages in
    Adder_subtractor_pipe.add
      ~scope
      ~clock
      ~enable:vdd
      ~stages:config.adder_stages
      [ a; b ]
    |> concat_result
    |> fun x ->
    let n = latency config - Adder_subtractor_pipe.latency ~stages in
    assert (n >= 0);
    if n = 0 then x else pipeline spec ~n ~enable:vdd x
  ;;

  let sub_pipe ~ensure_positive ~scope ~latency ~(config : Config.t) ~clock a b =
    assert (width a = width b);
    let width = width a in
    let spec = Reg_spec.create ~clock () in
    let stages = config.subtractor_stages in
    (if ensure_positive
    then
      Adder_subtractor_pipe.mixed
        ~scope
        ~clock
        ~enable:vdd
        ~stages
        ~init:a
        [ Sub b; Add (Signal.of_z config.p ~width) ]
    else Adder_subtractor_pipe.sub ~scope ~clock ~enable:vdd ~stages [ a; b ])
    |> concat_result
    |> fun x ->
    let n = latency config - Adder_subtractor_pipe.latency ~stages in
    assert (n >= 0);
    if n = 0 then x else pipeline spec ~n ~enable:vdd x
  ;;

  module Datapath_input = struct
    type 'a t =
      { p1 : 'a Xyzt.t
      ; p2 : 'a Xyt.t
      ; valid : 'a
      }
  end

  module Stage0 = struct
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
        add_pipe ~scope ~latency:(Fn.const config.adder_stages) ~config ~clock p1.y p1.x
      in
      let y1_minus_x1 =
        sub_pipe
          ~ensure_positive:true
          ~scope
          ~latency:(Fn.const config.adder_stages)
          ~config
          ~clock
          p1.y
          p1.x
        |> Fn.flip sel_bottom (num_bits + 1)
      in
      [%test_result: int] (width y1_plus_x1) ~expect:(num_bits + 1);
      [%test_result: int] (width y1_minus_x1) ~expect:(num_bits + 1);
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

    let latency_without_arbitration (config : Config.t) =
      Config.multiply_latency ~reduce:Coarse config
    ;;

    let latency (config : Config.t) =
      latency_without_arbitration config + if config.arbitrated_multiplier then 1 else 0
    ;;

    let create ~config ~scope ~clock { Stage0.p1; p2; y1_plus_x1; y1_minus_x1; valid } =
      [%test_result: int] (width y1_plus_x1) ~expect:(num_bits + 1);
      [%test_result: int] (width y1_minus_x1) ~expect:(num_bits + 1);
      [%test_result: int] (width p2.x) ~expect:num_bits;
      [%test_result: int] (width p2.y) ~expect:num_bits;
      let include_fine_reduction = false in
      let spec = Reg_spec.create ~clock () in
      let pipe = pipeline spec ~n:(latency config) in
      let x2 = uresize p2.x (num_bits + 1) in
      let y2 = uresize p2.y (num_bits + 1) in
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

    let latency (config : Config.t) = config.adder_stages

    let create ~config ~scope ~clock { Stage1.c_A; c_B; c_C; c_D; valid } =
      [%test_result: int] (width c_A) ~expect:(num_bits + num_correction_steps + 1);
      [%test_result: int] (width c_B) ~expect:(num_bits + num_correction_steps + 1);
      [%test_result: int] (width c_C) ~expect:(num_bits + num_correction_steps);
      [%test_result: int] (width c_D) ~expect:num_bits;
      let c_C = uresize c_C (num_bits + num_correction_steps + 1) in
      let c_D = uresize c_D (num_bits + num_correction_steps + 1) in
      let spec = Reg_spec.create ~clock () in
      let pipe = pipeline spec ~n:(latency config) in
      (* Consider arb-ing here? *)
      let c_E = sub_pipe ~ensure_positive:true ~scope ~latency ~config ~clock c_B c_A in
      let c_F = sub_pipe ~ensure_positive:true ~scope ~latency ~config ~clock c_D c_C in
      let c_G = add_pipe ~scope ~latency ~config ~clock c_D c_C in
      let c_H = add_pipe ~scope ~latency ~config ~clock c_B c_A in
      let scope = Scope.sub_scope scope "stage2" in
      { c_E; c_F; c_G; c_H; valid = pipe valid }
      |> map2 port_names ~f:(fun name x -> Scope.naming scope x name)
    ;;
  end

  module Stage2_reduce = struct
    type 'a t =
      { c_E : 'a [@bits num_bits]
      ; c_F : 'a [@bits num_bits]
      ; c_G : 'a [@bits num_bits]
      ; c_H : 'a [@bits num_bits]
      ; valid : 'a
      }
    [@@deriving sexp_of, hardcaml]

    let latency (config : Config.t) = (2 * config.adder_stages) + 1

    let reduce ~(build_mode : Build_mode.t) ~(config : Config.t) ~scope ~clock v =
      assert (Z.(equal Field_ops_model.Approx_msb_multiplier_model.p config.p));
      let spec = Reg_spec.create ~clock () in
      (* build the static bram values *)
      let log2_depth = 9 in
      let read_latency = 1 in
      let mux_list =
        (* we can knock off the top [log2_depth] bits here because we know what they are *)
        let tbl =
          Field_ops_model.Approx_msb_multiplier_model.build_precompute_two log2_depth
        in
        List.init (1 lsl log2_depth) ~f:(fun i ->
          Hashtbl.find_exn tbl i
          |> Signal.of_z ~width:(num_bits + log2_depth)
          |> Fn.flip Signal.drop_top log2_depth)
      in
      (* make sure the inputs are good *)
      let num_extra_bits = width v - num_bits in
      assert (num_extra_bits <= log2_depth);
      [%test_result: int] num_extra_bits ~expect:(num_correction_steps + 2);
      (* do a coarse reduction from [0,512M] to [0,4M) - in our particular case, it's
       * actually just [0, 3M) *)
      let rd_idx = uresize (sel_top v num_extra_bits) log2_depth in
      let bram =
        match build_mode with
        | Simulation -> mux rd_idx mux_list |> pipeline spec ~n:read_latency
        | Synthesis ->
          (* CR rahul: do an init bram here *)
          Signal.gnd
      in
      let coarse_reduction =
        let v = pipeline spec ~n:read_latency (drop_top v num_extra_bits) in
        assert (width v = width bram);
        sub_pipe
          ~ensure_positive:false
          ~scope
          ~latency:(Fn.const config.adder_stages)
          ~config
          ~clock
          v
          bram
      in
      [%test_result: int] (width coarse_reduction) ~expect:(num_bits + 1);
      (* do a fine reduction *)
      let fine_reduction =
        List.map [ 2; 1 ] ~f:(fun i ->
          let sub_val = Signal.of_z Z.(of_int i * config.p) ~width:(num_bits + 1) in
          let res =
            sub_pipe
              ~ensure_positive:false
              ~scope
              ~latency:(Fn.const config.adder_stages)
              ~config
              ~clock
              coarse_reduction
              sub_val
          in
          { With_valid.valid = ~:(msb res); value = lsbs res })
        |> priority_select_with_default ~default:coarse_reduction
      in
      uresize fine_reduction num_bits
    ;;

    let create ~build_mode ~config ~scope ~clock { Stage2.c_E; c_F; c_G; c_H; valid } =
      [%test_result: int] (width c_E) ~expect:(num_bits + num_correction_steps + 2);
      [%test_result: int] (width c_F) ~expect:(num_bits + num_correction_steps + 2);
      [%test_result: int] (width c_G) ~expect:(num_bits + num_correction_steps + 2);
      [%test_result: int] (width c_H) ~expect:(num_bits + num_correction_steps + 2);
      let spec = Reg_spec.create ~clock () in
      let pipe = pipeline spec ~n:(latency config) in
      (* Consider arb-ing here? *)
      let c_E = reduce c_E ~build_mode ~config ~scope ~clock in
      let c_F = reduce c_F ~build_mode ~config ~scope ~clock in
      let c_G = reduce c_G ~build_mode ~config ~scope ~clock in
      let c_H = reduce c_H ~build_mode ~config ~scope ~clock in
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
