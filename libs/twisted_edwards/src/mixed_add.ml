open Core
open Hardcaml
open Signal

include struct
  open Field_ops_lib
  module Arbitrate = Arbitrate
  module Modulo_adder_pipe = Modulo_adder_pipe
  module Modulo_subtractor_pipe = Modulo_subtractor_pipe
  module Modulo_double_pipe = Modulo_double_pipe
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
      ; clear : 'a
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
    ~clear
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
    let scope = Scope.sub_scope scope "multiply" in
    Arbitrate.arbitrate2
      (x1 @: y1, x2 @: y2)
      ~enable
      ~clock
      ~clear
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

  let double_pipe ~scope ~latency ~(config : Config.t) ~clock a =
    let spec = Reg_spec.create ~clock () in
    let stages = config.doubler_stages in
    Modulo_double_pipe.hierarchical ~scope ~clock ~enable:vdd ~stages ~p:config.p a
    |> fun x ->
    let n = latency config - Modulo_double_pipe.latency ~stages in
    assert (n >= 0);
    if n = 0 then x else pipeline spec ~n ~enable:vdd x
  ;;

  let of_z x = Signal.of_z ~width:num_bits x

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
      ; y2_plus_x2 : 'a
      ; y2_minus_x2 : 'a
      ; valid : 'a
      }
    [@@deriving sexp_of, hardcaml]

    let latency_without_arbitration (config : Config.t) = config.adder_stages
    let latency (config : Config.t) = latency_without_arbitration config

    let create ~config ~scope ~clock ~clear { Datapath_input.p1; p2; valid } =
      let spec = Reg_spec.create ~clock () in
      let pipe = pipeline spec ~n:(latency config) in
      let spec_with_clear = Reg_spec.create ~clock ~clear () in
      let pipe_with_clear = pipeline spec_with_clear ~n:(latency config) in
      let y1_plus_x1 =
        add_pipe ~scope ~latency:(Fn.const config.adder_stages) ~config ~clock p1.y p1.x
      in
      let y1_minus_x1 =
        sub_pipe ~scope ~latency:(Fn.const config.adder_stages) ~config ~clock p1.y p1.x
      in
      let y2_plus_x2 =
        add_pipe ~scope ~latency:(Fn.const config.adder_stages) ~config ~clock p2.y p2.x
      in
      let y2_minus_x2 =
        sub_pipe ~scope ~latency:(Fn.const config.adder_stages) ~config ~clock p2.y p2.x
      in
      let scope = Scope.sub_scope scope "stage0" in
      { y1_plus_x1
      ; y1_minus_x1
      ; y2_plus_x2
      ; y2_minus_x2
      ; p1 = Xyzt.map ~f:pipe p1
      ; p2 = Xyt.map ~f:pipe p2
      ; valid = pipe_with_clear valid
      }
      |> map2 port_names ~f:(fun name x -> Scope.naming scope x name)
    ;;
  end

  module Stage1 = struct
    type 'a t =
      { c_A : 'a [@bits num_bits]
      ; t1_times_t2 : 'a [@bits num_bits]
      ; c_D : 'a [@bits num_bits]
      ; y1_plus_x1 : 'a [@bits num_bits]
      ; y2_plus_x2 : 'a [@bits num_bits]
      ; valid : 'a
      }
    [@@deriving sexp_of, hardcaml]

    let latency_without_arbitration (config : Config.t) =
      Config.multiply_latency ~reduce:true config
    ;;

    let latency (config : Config.t) = latency_without_arbitration config + 1

    let create
      ~config
      ~scope
      ~clock
      ~clear
      { Stage0.p1; p2; y1_plus_x1; y1_minus_x1; y2_plus_x2; y2_minus_x2; valid }
      =
      let spec = Reg_spec.create ~clock () in
      let pipe = pipeline spec ~n:(latency config) in
      let spec_with_clear = Reg_spec.create ~clock ~clear () in
      let pipe_with_clear = pipeline spec_with_clear ~n:(latency config) in
      let c_A, t1_times_t2 =
        arbitrate_multiply
          ~config
          ~scope
          ~clock
          ~clear
          ~valid
          ~latency_without_arbitration
          (y1_minus_x1, y2_minus_x2)
          (p1.t, p2.t)
      in
      let c_D = double_pipe ~scope ~latency ~config ~clock p1.z in
      let scope = Scope.sub_scope scope "stage1" in
      { c_A
      ; t1_times_t2
      ; c_D
      ; y1_plus_x1 = pipe y1_plus_x1
      ; y2_plus_x2 = pipe y2_plus_x2
      ; valid = pipe_with_clear valid
      }
      |> map2 port_names ~f:(fun name x -> Scope.naming scope x name)
    ;;
  end

  module Stage2 = struct
    type 'a t =
      { c_A : 'a [@bits num_bits]
      ; c_B : 'a [@bits num_bits]
      ; c_C : 'a [@bits num_bits]
      ; c_D : 'a [@bits num_bits]
      ; valid : 'a
      }
    [@@deriving sexp_of, hardcaml]

    let latency_without_arbitration (config : Config.t) =
      Config.multiply_latency ~reduce:true config
    ;;

    let latency (config : Config.t) = latency_without_arbitration config + 1

    let create
      ~config
      ~scope
      ~clock
      ~clear
      { Stage1.t1_times_t2; c_A; y1_plus_x1; y2_plus_x2; c_D; valid }
      =
      let spec = Reg_spec.create ~clock () in
      let pipe = pipeline spec ~n:(latency config) in
      let spec_with_clear = Reg_spec.create ~clock ~clear () in
      let pipe_with_clear = pipeline spec_with_clear ~n:(latency config) in
      let c_C, c_B =
        let k = Modulo_ops.(config.d * of_int 2) in
        arbitrate_multiply
          ~config
          ~scope
          ~clock
          ~clear
          ~valid
          ~latency_without_arbitration
          (t1_times_t2, of_z k)
          (y1_plus_x1, y2_plus_x2)
      in
      let scope = Scope.sub_scope scope "stage2" in
      { c_A = pipe c_A; c_B; c_C; c_D = pipe c_D; valid = pipe_with_clear valid }
      |> map2 port_names ~f:(fun name x -> Scope.naming scope x name)
    ;;
  end

  module Stage3 = struct
    type 'a t =
      { c_E : 'a [@bits num_bits]
      ; c_F : 'a [@bits num_bits]
      ; c_G : 'a [@bits num_bits]
      ; c_H : 'a [@bits num_bits]
      ; valid : 'a
      }
    [@@deriving sexp_of, hardcaml]

    let latency_without_arbitration (config : Config.t) = config.adder_stages
    let latency (config : Config.t) = latency_without_arbitration config

    let create ~config ~scope ~clock ~clear { Stage2.c_A; c_B; c_C; c_D; valid } =
      let spec_with_clear = Reg_spec.create ~clock ~clear () in
      let pipe_with_clear = pipeline spec_with_clear ~n:(latency config) in
      (* Consider arb-ing here? *)
      let c_E = sub_pipe ~scope ~latency ~config ~clock c_B c_A in
      let c_F = sub_pipe ~scope ~latency ~config ~clock c_D c_C in
      let c_G = add_pipe ~scope ~latency ~config ~clock c_D c_C in
      let c_H = add_pipe ~scope ~latency ~config ~clock c_B c_A in
      let scope = Scope.sub_scope scope "stage3" in
      { c_E; c_F; c_G; c_H; valid = pipe_with_clear valid }
      |> map2 port_names ~f:(fun name x -> Scope.naming scope x name)
    ;;
  end

  module Stage4 = struct
    type 'a t =
      { x3 : 'a
      ; y3 : 'a
      ; z3 : 'a
      ; t3 : 'a
      ; valid : 'a
      }
    [@@deriving sexp_of, hardcaml]

    let latency_without_arbitration (config : Config.t) =
      Config.multiply_latency ~reduce:true config
    ;;

    let latency (config : Config.t) = latency_without_arbitration config + 1

    let create ~config ~scope ~clock ~clear { Stage3.c_E; c_F; c_G; c_H; valid } =
      let spec_with_clear = Reg_spec.create ~clock ~clear () in
      let pipe_with_clear = pipeline spec_with_clear ~n:(latency config) in
      let x3, y3 =
        arbitrate_multiply
          ~config
          ~scope
          ~clock
          ~clear
          ~valid
          ~latency_without_arbitration
          (c_E, c_F)
          (c_G, c_H)
      in
      let t3, z3 =
        arbitrate_multiply
          ~config
          ~scope
          ~clock
          ~clear
          ~valid
          ~latency_without_arbitration
          (c_E, c_H)
          (c_F, c_G)
      in
      let scope = Scope.sub_scope scope "stage4" in
      { x3; y3; z3; t3; valid = pipe_with_clear valid }
      |> map2 port_names ~f:(fun name x -> Scope.naming scope x name)
    ;;
  end

  module Stage5 = struct
    type 'a t =
      { x3 : 'a
      ; y3 : 'a
      ; z3 : 'a
      ; t3 : 'a
      ; valid : 'a
      }
    [@@deriving sexp_of, hardcaml]

    let latency (config : Config.t) = config.output_pipeline_stages

    let create ~scope ~clock ~clear ~config { Stage4.x3; y3; z3; t3; valid } =
      assert (latency config >= 0);
      let scope = Scope.sub_scope scope "stage4" in
      { x3; y3; z3; t3; valid }
      |> map ~f:(pipeline (Reg_spec.create ~clock ~clear ()) ~n:(latency config))
      |> map ~f:(fun x ->
           if latency config > 0
           then
             (* Only mark as dont_touch if there is any pipelining. *)
             add_attribute x (Rtl_attribute.Vivado.dont_touch true)
           else x)
      |> map2 port_names ~f:(fun name x -> Scope.naming scope x name)
    ;;
  end

  let latency config =
    Stage0.latency config
    + Stage1.latency config
    + Stage2.latency config
    + Stage3.latency config
    + Stage4.latency config
    + Stage5.latency config
  ;;

  let create ~config scope { I.clock; clear; valid_in; p1; p2 } =
    let { Stage5.x3; y3; z3; t3; valid = valid_out } =
      { p1; p2; valid = valid_in }
      |> Stage0.create ~config ~scope ~clock ~clear
      |> Stage1.create ~config ~scope ~clock ~clear
      |> Stage2.create ~config ~scope ~clock ~clear
      |> Stage3.create ~config ~scope ~clock ~clear
      |> Stage4.create ~config ~scope ~clock ~clear
      |> Stage5.create ~config ~scope ~clock ~clear
    in
    { O.valid_out; p3 = { x = x3; y = y3; z = z3; t = t3 } }
  ;;
end