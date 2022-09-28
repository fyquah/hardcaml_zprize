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

  let multiply ~(config : Config.t) ~scope ~clock ~latency (x, y) =
    let enable = vdd in
    assert (width x = width y);
    let scope = Scope.sub_scope scope "multiply" in
    config.multiply.impl ~scope ~clock ~enable x (Some y)
    |> Config.reduce config ~scope ~clock ~enable
    |> pipeline
         (Reg_spec.create ~clock ())
         ~enable
         ~n:(latency config - Config.multiply_latency ~reduce:true config)
  ;;

  let add_pipe ~scope ~latency ~(config : Config.t) ~clock a b =
    let spec = Reg_spec.create ~clock () in
    let stages = config.adder_stages in
    Modulo_adder_pipe.hierarchical ~scope ~clock ~enable:vdd ~stages ~p:config.p a b
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
      { p1 : 'a Xyzt.t
      ; p2 : 'a Xyt.t
      ; subtract : 'a
      ; valid : 'a
      }
  end

  module Stage0 = struct
    type 'a t =
      { p1 : 'a Xyzt.t [@rtlprefix "p1$"]
      ; p2 : 'a Xyt.t [@rtlprefix "p2$"]
      ; y1_plus_x1 : 'a
      ; y1_minus_x1 : 'a
      ; subtract : 'a
      ; valid : 'a
      }
    [@@deriving sexp_of, hardcaml]

    let latency (config : Config.t) = config.adder_stages

    let create ~config ~scope ~clock { Datapath_input.p1; p2; subtract; valid } =
      let spec = Reg_spec.create ~clock () in
      let pipe = pipeline spec ~n:(latency config) in
      let y1_plus_x1 = add_pipe ~scope ~latency ~config ~clock p1.y p1.x in
      let y1_minus_x1 = sub_pipe ~scope ~latency ~config ~clock p1.y p1.x in
      let scope = Scope.sub_scope scope "stage0" in
      { y1_plus_x1
      ; y1_minus_x1
      ; p1 = Xyzt.map ~f:pipe p1
      ; p2 = Xyt.map ~f:pipe p2
      ; subtract = pipe subtract
      ; valid = pipe valid
      }
      |> map2 port_names ~f:(fun name x -> Scope.naming scope x name)
    ;;
  end

  module Stage1 = struct
    type 'a t =
      { c_A : 'a [@bits num_bits]
      ; c_B : 'a [@bits num_bits]
      ; c_C : 'a [@bits num_bits]
      ; c_D : 'a [@bits num_bits]
      ; subtract : 'a
      ; valid : 'a
      }
    [@@deriving sexp_of, hardcaml]

    let latency_without_arbitration (config : Config.t) =
      Config.multiply_latency ~reduce:true config
    ;;

    let latency (config : Config.t) =
      latency_without_arbitration config + if config.arbitrated_multiplier then 1 else 0
    ;;

    let create
      ~config
      ~scope
      ~clock
      { Stage0.p1; p2; y1_plus_x1; y1_minus_x1; subtract; valid }
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
          ( multiply ~latency ~config ~scope ~clock (y1_minus_x1, p2.x)
          , multiply ~latency ~config ~scope ~clock (y1_plus_x1, p2.y) )
      in
      let c_C = multiply ~latency ~config ~scope ~clock (p1.t, p2.t) in
      let scope = Scope.sub_scope scope "stage1" in
      { c_A; c_B; c_C; c_D = pipe p1.z; subtract = pipe subtract; valid = pipe valid }
      |> map2 port_names ~f:(fun name x -> Scope.naming scope x name)
    ;;
  end

  module Stage2 = struct
    type 'a t =
      { c_E : 'a [@bits num_bits]
      ; c_F : 'a [@bits num_bits]
      ; c_G : 'a [@bits num_bits]
      ; c_H : 'a [@bits num_bits]
      ; valid : 'a
      }
    [@@deriving sexp_of, hardcaml]

    let latency (config : Config.t) = config.adder_stages

    let create ~config ~scope ~clock { Stage1.c_A; c_B; c_C; c_D; subtract; valid } =
      let spec = Reg_spec.create ~clock () in
      let pipe = pipeline spec ~n:(latency config) in
      (* Consider arb-ing here? *)
      let c_E = sub_pipe ~scope ~latency ~config ~clock c_B c_A in
      let c_D_minus_c_C = sub_pipe ~scope ~latency ~config ~clock c_D c_C in
      let c_D_plus_c_C = add_pipe ~scope ~latency ~config ~clock c_D c_C in
      let c_H = add_pipe ~scope ~latency ~config ~clock c_B c_A in
      (* assign based on the sign of t2 *)
      let subtract = pipe subtract in
      let c_F = mux2 subtract c_D_plus_c_C c_D_minus_c_C in
      let c_G = mux2 subtract c_D_minus_c_C c_D_plus_c_C in
      let scope = Scope.sub_scope scope "stage2" in
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
      Config.multiply_latency ~reduce:true config
    ;;

    let latency (config : Config.t) =
      latency_without_arbitration config + if config.arbitrated_multiplier then 1 else 0
    ;;

    let create ~config ~scope ~clock { Stage2.c_E; c_F; c_G; c_H; valid } =
      let spec = Reg_spec.create ~clock () in
      let pipe = pipeline spec ~n:(latency config) in
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
          ( multiply ~latency ~config ~scope ~clock (c_E, c_F)
          , multiply ~latency ~config ~scope ~clock (c_G, c_H) )
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
          ( multiply ~latency ~config ~scope ~clock (c_E, c_H)
          , multiply ~latency ~config ~scope ~clock (c_F, c_G) )
      in
      let scope = Scope.sub_scope scope "stage3" in
      { x3; y3; z3; t3; valid = pipe valid }
      |> map2 port_names ~f:(fun name x -> Scope.naming scope x name)
    ;;
  end

  let output_pipes = 2

  let latency config =
    Stage0.latency config
    + Stage1.latency config
    + Stage2.latency config
    + Stage3.latency config
    + output_pipes
  ;;

  let create ~config scope { I.clock; valid_in; p1; p2; subtract } =
    let { Stage3.x3; y3; z3; t3; valid = valid_out } =
      (* when we subtract, we want to output [p1-p2] instead of [p1+p2]. Because p2 is coming
       * from the host, it is represented as ((y-x)/2, (y+x)/2, 4d * t) -> negating a point
       * in twisted edwards is equivalent to negating the x coordinate, so we have to swap x,y and
       * negate t. We push the t negation down the computation to avoid adding another modulo 
       * subtractor. *)
      let p2 = Xyt.Of_signal.mux2 subtract { Xyt.x = p2.y; y = p2.x; t = p2.t } p2 in
      { p1; p2; subtract; valid = valid_in }
      |> Stage0.create ~config ~scope ~clock
      |> Stage1.create ~config ~scope ~clock
      |> Stage2.create ~config ~scope ~clock
      |> Stage3.create ~config ~scope ~clock
      |> Stage3.Of_signal.pipeline ~n:output_pipes (Reg_spec.create ~clock ())
    in
    { O.valid_out; p3 = { x = x3; y = y3; z = z3; t = t3 } }
  ;;

  let hierarchical ?instance ~config scope =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ?instance ~name:"adder_precompute" ~scope (create ~config)
  ;;
end
