open Core
open Hardcaml
open Signal

include struct
  open Field_ops_lib
  module Arbitrate = Arbitrate
  module Modulo_adder_pipe = Modulo_adder_pipe
  module Modulo_subtractor_pipe = Modulo_subtractor_pipe
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

  let arbitrate_multiply
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
        config.multiply.impl ~scope ~clock ~enable x (Some y)
        |> Config.reduce config ~scope ~clock ~enable
        |> pipeline
             (Reg_spec.create ~clock ())
             ~enable
             ~n:
               (latency_without_arbitration config
               - Config.multiply_latency ~reduce:true config))
  ;;

  let multiply ~(config : Config.t) ~scope ~clock ~latency_without_arbitration (x1, y1) =
    let enable = vdd in
    assert (width y1 = width y1);
    let wy = width y1 in
    let wx = width x1 in
    let scope = Scope.sub_scope scope "multiply" in
    let y = sel_bottom y1 wy in
    let x = sel_top x1 wx in
    config.multiply.impl ~scope ~clock ~enable x (Some y)
    |> Config.reduce config ~scope ~clock ~enable
    |> pipeline
         (Reg_spec.create ~clock ())
         ~enable
         ~n:
           (latency_without_arbitration config
           - Config.multiply_latency ~reduce:true config)
  ;;

  let add_pipe ~scope ~latency ~(config : Config.t) ~clock a b =
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

  let sub_pipe ~scope:_ ~latency ~(config : Config.t) ~clock a b =
    let spec = Reg_spec.create ~clock () in
    let stages = config.subtractor_stages in
    Modulo_subtractor_pipe.create ~clock ~enable:vdd ~stages ~p:config.p a b
    |> fun x ->
    let n = latency config - Modulo_adder_pipe.latency ~stages in
    assert (n >= 0);
    if n = 0 then x else pipeline spec ~n ~enable:vdd x
  ;;

  module Datapath_input = struct
    type 'a t =
      { p1 : 'a Xyzt.t [@rtlprefix "p1$"]
      ; p2 : 'a Xyt.t [@rtlprefix "p2$"]
      ; valid : 'a [@rtlname "input_valid"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Stage0 = struct
    module I = struct
      type 'a t =
        { clock : 'a
        ; datapath_input : 'a Datapath_input.t
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t =
        { p1 : 'a Xyzt.t [@rtlprefix "o_p1$"]
        ; p2 : 'a Xyt.t [@rtlprefix "o_p2$"]
        ; y1_plus_x1 : 'a [@bits num_bits]
        ; y1_minus_x1 : 'a [@bits num_bits]
        ; valid : 'a [@rtlname "stage0_valid"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    let latency_without_arbitration (config : Config.t) = config.adder_stages
    let latency (config : Config.t) = latency_without_arbitration config

    let create
      ~config
      scope
      { I.clock; datapath_input = { Datapath_input.p1; p2; valid } }
      =
      let spec = Reg_spec.create ~clock () in
      let pipe = pipeline spec ~n:(latency config) in
      let y1_plus_x1 =
        add_pipe ~scope ~latency:(Fn.const config.adder_stages) ~config ~clock p1.y p1.x
      in
      let y1_minus_x1 =
        sub_pipe ~scope ~latency:(Fn.const config.adder_stages) ~config ~clock p1.y p1.x
      in
      let scope = Scope.sub_scope scope "stage0" in
      { O.y1_plus_x1
      ; y1_minus_x1
      ; p1 = Xyzt.map ~f:pipe p1
      ; p2 = Xyt.map ~f:pipe p2
      ; valid = pipe valid
      }
      |> O.map2 O.port_names ~f:(fun name x -> Scope.naming scope x name)
    ;;

    let hierarchical ~config scope i =
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical ~scope ~name:"mixed_add_precompute_stage_0" (create ~config) i
    ;;
  end

  module Stage1a = struct
    module I = struct
      type 'a t =
        { clock : 'a
        ; stage0 : 'a Stage0.O.t
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t =
        { valid : 'a [@rtlname "stage_1avalid"]
        ; c_A : 'a [@bits num_bits]
        ; c_B : 'a [@bits num_bits]
        }
      [@@deriving sexp_of, hardcaml]
    end

    let latency_without_arbitration (config : Config.t) =
      Config.multiply_latency ~reduce:true config
    ;;

    let latency (config : Config.t) =
      latency_without_arbitration config + if config.arbitrated_multiplier then 1 else 0
    ;;

    let create
      ~config
      scope
      { I.clock; stage0 = { p1 = _; p2; y1_plus_x1; y1_minus_x1; valid } }
      =
      let spec = Reg_spec.create ~clock () in
      let pipe = pipeline spec ~n:(latency config) in
      let c_A, c_B =
        if config.arbitrated_multiplier
        then
          arbitrate_multiply
            ~config
            ~scope
            ~clock
            ~valid
            ~latency_without_arbitration
            (y1_minus_x1, p2.x)
            (y1_plus_x1, p2.y)
        else
          ( multiply ~latency_without_arbitration ~config ~scope ~clock (y1_minus_x1, p2.x)
          , multiply ~latency_without_arbitration ~config ~scope ~clock (y1_plus_x1, p2.y)
          )
      in
      let scope = Scope.sub_scope scope "stage1a" in
      { c_A; c_B; valid = pipe valid }
      |> O.map2 O.port_names ~f:(fun name x -> Scope.naming scope x name)
    ;;

    let hierarchical ~config scope i =
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical ~scope ~name:"mixed_add_precompute_stage_1a" (create ~config) i
    ;;
  end

  module Stage1b = struct
    module I = struct
      type 'a t =
        { clock : 'a
        ; stage0 : 'a Stage0.O.t
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t =
        { c_C : 'a [@bits num_bits]
        ; c_D : 'a [@bits num_bits]
        ; valid : 'a [@rtlname "stage1_valid"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    let latency_without_arbitration (config : Config.t) =
      Config.multiply_latency ~reduce:true config
    ;;

    let latency (config : Config.t) =
      latency_without_arbitration config + if config.arbitrated_multiplier then 1 else 0
    ;;

    let create
      ~config
      scope
      { I.clock; stage0 = { p1; p2; y1_plus_x1 = _; y1_minus_x1 = _; valid } }
      =
      let spec = Reg_spec.create ~clock () in
      let pipe = pipeline spec ~n:(latency config) in
      let c_C =
        config.multiply.impl ~scope ~clock ~enable:vdd p1.t (Some p2.t)
        |> Config.reduce config ~scope ~clock ~enable:vdd
        |> pipeline
             (Reg_spec.create ~clock ())
             ~enable:vdd
             ~n:(latency config - Config.multiply_latency ~reduce:true config)
      in
      let scope = Scope.sub_scope scope "stage1b" in
      { c_C; c_D = pipe p1.z; valid = pipe valid }
      |> O.map2 O.port_names ~f:(fun name x -> Scope.naming scope x name)
    ;;

    let hierarchical ~config scope i =
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical ~scope ~name:"mixed_add_precompute_stage_1b" (create ~config) i
    ;;
  end

  module Stage2 = struct
    module I = struct
      type 'a t =
        { clock : 'a
        ; stage1a : 'a Stage1a.O.t [@rtlprefix "stage1a$"]
        ; stage1b : 'a Stage1b.O.t [@rtlprefix "stage1b$"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t =
        { c_E : 'a [@bits num_bits]
        ; c_F : 'a [@bits num_bits]
        ; c_G : 'a [@bits num_bits]
        ; c_H : 'a [@bits num_bits]
        ; valid : 'a [@rtlname "stage2_valid"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    let latency_without_arbitration (config : Config.t) = config.adder_stages
    let latency (config : Config.t) = latency_without_arbitration config

    let create
      ~config
      scope
      { I.clock; stage1a = { c_A; c_B; valid }; stage1b = { c_C; c_D; valid = _ } }
      =
      let spec = Reg_spec.create ~clock () in
      let pipe = pipeline spec ~n:(latency config) in
      (* Consider arb-ing here? *)
      let c_E = sub_pipe ~scope ~latency ~config ~clock c_B c_A in
      let c_F = sub_pipe ~scope ~latency ~config ~clock c_D c_C in
      let c_G = add_pipe ~scope ~latency ~config ~clock c_D c_C in
      let c_H = add_pipe ~scope ~latency ~config ~clock c_B c_A in
      let scope = Scope.sub_scope scope "stage2" in
      { c_E; c_F; c_G; c_H; valid = pipe valid }
      |> O.map2 O.port_names ~f:(fun name x -> Scope.naming scope x name)
    ;;

    let hierarchical ~config scope i =
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical ~scope ~name:"mixed_add_precompute_stage_2" (create ~config) i
    ;;
  end

  module Stage3 = struct
    module I = struct
      type 'a t =
        { clock : 'a
        ; stage2 : 'a Stage2.O.t
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t =
        { x3 : 'a [@bits num_bits]
        ; y3 : 'a [@bits num_bits]
        ; z3 : 'a [@bits num_bits]
        ; t3 : 'a [@bits num_bits]
        ; valid : 'a [@rtlname "stage3_valid"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    let latency_without_arbitration (config : Config.t) =
      Config.multiply_latency ~reduce:true config
    ;;

    let latency (config : Config.t) =
      latency_without_arbitration config + if config.arbitrated_multiplier then 1 else 0
    ;;

    let create ~config scope { I.clock; stage2 = { c_E; c_F; c_G; c_H; valid } } =
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
            (c_E, c_F)
            (c_G, c_H)
        else
          ( multiply ~latency_without_arbitration ~config ~scope ~clock (c_E, c_F)
          , multiply ~latency_without_arbitration ~config ~scope ~clock (c_G, c_H) )
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
            (c_E, c_H)
            (c_F, c_G)
        else
          ( multiply ~latency_without_arbitration ~config ~scope ~clock (c_E, c_H)
          , multiply ~latency_without_arbitration ~config ~scope ~clock (c_F, c_G) )
      in
      let scope = Scope.sub_scope scope "stage3" in
      { x3; y3; z3; t3; valid = pipe_with_clear valid }
      |> O.map2 O.port_names ~f:(fun name x -> Scope.naming scope x name)
    ;;

    let hierarchical ~config scope i =
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical ~scope ~name:"mixed_add_precompute_stage_3" (create ~config) i
    ;;
  end

  let output_pipes = 2

  let latency config =
    assert (Stage1a.latency config = Stage1b.latency config);
    Stage0.latency config
    + Stage1a.latency config
    + Stage2.latency config
    + Stage3.latency config
    + output_pipes
  ;;

  let create ~config scope { I.clock; valid_in; p1; p2 } =
    let { Stage3.O.x3; y3; z3; t3; valid = valid_out } =
      let datapath_input = { Datapath_input.p1; p2; valid = valid_in } in
      let stage0 = Stage0.hierarchical ~config scope { clock; datapath_input } in
      let stage1a = Stage1a.hierarchical ~config scope { clock; stage0 } in
      let stage1b = Stage1b.hierarchical ~config scope { clock; stage0 } in
      let stage2 = Stage2.hierarchical ~config scope { clock; stage1a; stage1b } in
      let stage3 = Stage3.hierarchical ~config scope { clock; stage2 } in
      Stage3.O.Of_signal.pipeline ~n:output_pipes (Reg_spec.create ~clock ()) stage3
    in
    { O.valid_out; p3 = { x = x3; y = y3; z = z3; t = t3 } }
  ;;

  let hierarchical ?instance ~config scope =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ?instance ~name:"adder_precompute" ~scope (create ~config)
  ;;
end
