open! Base
open! Hardcaml
open! Signal
open! Field_ops_lib
open! Reg_with_enable
open! Point

let subtractor_stages = 3
let adder_stages = 3

module Config = Ec_fpn_ops_config

let double_pipe ~twice_width ~scope ~latency ~(config : Config.t) ~clock ~enable a =
  let stages = adder_stages in
  let spec = Reg_spec.create ~clock () in
  let logr = Z.log2up config.p in
  a
  |> Modulo_double_pipe.hierarchical
       ~scope
       ~stages
       ~p:(if twice_width then Z.(config.p lsl logr) else config.p)
       ~clock
       ~enable
  |> pipeline spec ~enable ~n:(latency config - Modulo_double_pipe.latency ~stages)
;;

let sub_pipe ~latency ~(config : Config.t) ~clock ~enable a b =
  let spec = Reg_spec.create ~clock () in
  let stages = subtractor_stages in
  Modulo_subtractor_pipe.create ~clock ~enable ~stages:subtractor_stages ~p:config.p a b
  |> pipeline spec ~n:(latency config - Modulo_subtractor_pipe.latency ~stages) ~enable
;;

let arbitrate_square
  ~(config : Config.t)
  ~scope
  ~enable
  ~clock
  ~valid
  ~latency_without_arbitration
  x1
  x2
  =
  let scope = Scope.sub_scope scope "square" in
  Arbitrate.arbitrate2 (x1, x2) ~enable ~clock ~valid ~f:(fun x ->
    config.square.impl ~scope ~clock ~enable x None
    |> Config.reduce config ~scope ~clock ~enable
    |> pipeline
         (Reg_spec.create ~clock ())
         ~enable
         ~n:
           (latency_without_arbitration config - Config.square_latency ~reduce:true config))
;;

let arbitrate_multiply
  ~(config : Config.t)
  ~scope
  ~enable
  ~clock
  ~valid
  ~latency_without_arbitration
  (x1, y1)
  (x2, y2)
  =
  assert (width y1 = width y2);
  assert (width x1 = width x2);
  let wy = width y1 in
  let wx = width x1 in
  let scope = Scope.sub_scope scope "multiply" in
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

module Stage0 = struct
  type 'a t =
    { valid_in : 'a [@rtlname "valid"]
    ; data_in0 : 'a Affine.t [@rtlprefix "data_in0_"]
    ; data_in1 : 'a Jacobian.t [@rtlprefix "data_in1_"]
    ; data_in1_z_squared : 'a (* The doubler is able to compute z_squared for us. *)
    }
  [@@deriving sexp_of, hardcaml]

  let name scope =
    map2 port_names ~f:(Fn.flip (Scope.naming (Scope.sub_scope scope "stage0")))
  ;;
end

module Stage1 = struct
  type 'a t =
    { y0 : 'a
    ; s2 : 'a
    ; u1 : 'a
    ; u2 : 'a
    ; z1 : 'a
    ; z1_cubed : 'a
    ; valid : 'a
    }
  [@@deriving sexp_of, hardcaml]

  let latency_without_arbitration (config : Config.t) =
    Config.multiply_latency ~reduce:true config
  ;;

  let latency (config : Config.t) = latency_without_arbitration config + 1

  let create
    ~scope
    ~clock
    ~enable
    (config : Config.t)
    ({ data_in0 = { x = x0; y = y0 }
     ; data_in1 = { x = x1; y = y1; z = z1 }
     ; data_in1_z_squared = z1_squared
     ; valid_in = valid
     } :
      _ Stage0.t)
    =
    let spec = Reg_spec.create ~clock () in
    let pipe = pipeline spec ~n:(latency config) ~enable in
    let z1_cubed, u1 =
      arbitrate_multiply
        ~config
        ~scope
        ~clock
        ~enable
        ~valid
        ~latency_without_arbitration
        (z1, z1_squared)
        (x0, z1_squared)
    in
    { s2 = pipe y1
    ; u1
    ; u2 = pipe x1
    ; z1 = pipe z1
    ; z1_cubed
    ; y0 = pipe y0
    ; valid = pipe valid
    }
    |> map2 port_names ~f:(Fn.flip (Scope.naming (Scope.sub_scope scope "stage1")))
  ;;
end

module Stage2 = struct
  type 'a t =
    { h : 'a
    ; s2 : 'a
    ; y0 : 'a
    ; u1 : 'a
    ; u2 : 'a
    ; z1 : 'a
    ; z1_cubed : 'a
    ; valid : 'a
    }
  [@@deriving sexp_of, hardcaml]

  let latency (_config : Config.t) =
    Modulo_subtractor_pipe.latency ~stages:subtractor_stages
  ;;

  let create
    ~scope
    ~clock
    ~enable
    (config : Config.t)
    { Stage1.s2; u1; u2; z1; z1_cubed; y0; valid }
    =
    let spec = Reg_spec.create ~clock () in
    let pipe = pipeline ~n:(latency config) ~enable spec in
    let sub_pipe = sub_pipe ~latency ~config ~enable ~clock in
    { h = sub_pipe u2 u1
    ; y0 = pipe y0
    ; z1_cubed = pipe z1_cubed
    ; u1 = pipe u1
    ; u2 = pipe u2
    ; z1 = pipe z1
    ; s2 = pipe s2
    ; valid = pipe valid
    }
    |> map2 port_names ~f:(Fn.flip (Scope.naming (Scope.sub_scope scope "stage2")))
  ;;
end

module Stage3 = struct
  type 'a t =
    { h : 'a
    ; s1 : 'a
    ; s2 : 'a
    ; u1 : 'a
    ; u2 : 'a
    ; z_out : 'a
    ; valid : 'a
    }
  [@@deriving sexp_of, hardcaml]

  let latency_without_arbitration (config : Config.t) =
    Config.multiply_latency ~reduce:true config
  ;;

  let latency (config : Config.t) = latency_without_arbitration config + 1

  let create
    ~scope
    ~clock
    ~enable
    (config : Config.t)
    { Stage2.y0; h; z1_cubed; u2; s2; u1; z1; valid }
    =
    let spec = Reg_spec.create ~clock () in
    let pipe = pipeline ~n:(latency config) ~enable spec in
    let s1, z_out =
      arbitrate_multiply
        ~config
        ~scope
        ~clock
        ~enable
        ~valid
        ~latency_without_arbitration
        (y0, z1_cubed)
        (h, z1)
    in
    { h = pipe h
    ; u1 = pipe u1
    ; u2 = pipe u2
    ; s1
    ; s2 = pipe s2
    ; z_out
    ; valid = pipe valid
    }
    |> map2 port_names ~f:(Fn.flip (Scope.naming (Scope.sub_scope scope "stage3")))
  ;;
end

module Stage4 = struct
  type 'a t =
    { z_out : 'a
    ; r : 'a
    ; h : 'a
    ; s1 : 'a
    ; u1 : 'a
    ; error : 'a
    ; valid : 'a
    }
  [@@deriving sexp_of, hardcaml]

  let latency_without_arbitration (_config : Config.t) =
    Modulo_subtractor_pipe.latency ~stages:subtractor_stages
  ;;

  let latency (config : Config.t) = latency_without_arbitration config + 1

  let create
    ~scope
    ~clock
    ~enable
    (config : Config.t)
    { Stage3.s2; h; u1; u2; s1; z_out; valid }
    =
    let spec = Reg_spec.create ~clock () in
    let sub_pipe = sub_pipe ~latency ~config ~enable ~clock in
    let pipe = pipeline spec ~n:(latency config) ~enable in
    { z_out = pipe z_out
    ; h = pipe h
    ; r = sub_pipe s2 s1
    ; s1 = pipe s1
    ; u1 = pipe u1
    ; error = pipe (s1 ==: s2 &: (u1 ==: u2))
    ; valid = pipe valid
    }
    |> map2 port_names ~f:(Fn.flip (Scope.naming (Scope.sub_scope scope "stage4")))
  ;;
end

module Stage5 = struct
  type 'a t =
    { r_squared : 'a
    ; h : 'a
    ; h_squared : 'a
    ; z_out : 'a
    ; r : 'a
    ; s1 : 'a
    ; u1 : 'a
    ; error : 'a
    ; valid : 'a
    }
  [@@deriving sexp_of, hardcaml]

  let latency_without_arbitration (config : Config.t) =
    Config.square_latency ~reduce:true config
  ;;

  let latency (config : Config.t) = latency_without_arbitration config + 1

  let create
    ~scope
    ~clock
    ~enable
    (config : Config.t)
    { Stage4.z_out; h; r; u1; s1 : 'a; error; valid }
    =
    let scope = Scope.sub_scope scope "stage5" in
    let spec = Reg_spec.create ~clock () in
    let pipe = pipeline spec ~n:(latency config) ~enable in
    let r_squared, h_squared =
      arbitrate_square
        ~config
        ~scope
        ~clock
        ~enable
        ~valid
        ~latency_without_arbitration
        r
        h
    in
    { r_squared
    ; h = pipe h
    ; h_squared
    ; z_out = pipe z_out
    ; r = pipe r
    ; u1 = pipe u1
    ; s1 = pipe s1
    ; error = pipe error
    ; valid = pipe valid
    }
    |> map2 port_names ~f:(Fn.flip (Scope.naming scope))
  ;;
end

module Stage6 = struct
  type 'a t =
    { r : 'a
    ; r_squared : 'a
    ; u1_times_h_squared : 'a
    ; h_cubed : 'a
    ; s1 : 'a
    ; z_out : 'a
    ; error : 'a
    ; valid : 'a
    }
  [@@deriving sexp_of, hardcaml]

  let latency_without_arbitration (config : Config.t) =
    Config.multiply_latency ~reduce:true config
  ;;

  let latency (config : Config.t) = latency_without_arbitration config + 1

  let create
    ~scope
    ~clock
    ~enable
    (config : Config.t)
    { Stage5.r_squared; h; h_squared; z_out; s1; r; u1; error; valid }
    =
    let scope = Scope.sub_scope scope "stage6" in
    let spec = Reg_spec.create ~clock () in
    let pipe = pipeline spec ~n:(latency config) ~enable in
    let u1_times_h_squared, h_cubed =
      arbitrate_multiply
        ~config
        ~scope
        ~clock
        ~enable
        ~valid
        ~latency_without_arbitration
        (u1, h_squared)
        (h_squared, h)
    in
    { r = pipe r
    ; r_squared = pipe r_squared
    ; u1_times_h_squared
    ; s1 = pipe s1
    ; z_out = pipe z_out
    ; h_cubed
    ; error = pipe error
    ; valid = pipe valid
    }
    |> map2 port_names ~f:(Fn.flip (Scope.naming scope))
  ;;
end

module Stage7 = struct
  type 'a t =
    { tmp0 : 'a
    ; tmp1 : 'a
    ; r : 'a
    ; u1_times_h_squared : 'a
    ; z_out : 'a
    ; s1 : 'a
    ; h_cubed : 'a
    ; error : 'a
    ; valid : 'a
    }
  [@@deriving sexp_of, hardcaml]

  let latency (_config : Config.t) =
    Int.max
      (Modulo_subtractor_pipe.latency ~stages:subtractor_stages)
      (Modulo_double_pipe.latency ~stages:subtractor_stages)
  ;;

  let create
    ~scope
    ~clock
    ~enable
    (config : Config.t)
    { Stage6.r; r_squared; u1_times_h_squared; s1; z_out; h_cubed; error; valid }
    =
    let scope = Scope.sub_scope scope "stage7" in
    let spec = Reg_spec.create ~clock () in
    let pipe = pipeline spec ~n:(latency config) ~enable in
    let sub_pipe = sub_pipe ~latency ~config ~enable ~clock in
    let double_pipe = double_pipe ~scope ~latency ~config ~clock ~enable in
    { tmp0 = sub_pipe r_squared h_cubed
    ; tmp1 = double_pipe u1_times_h_squared ~twice_width:false
    ; u1_times_h_squared = pipe u1_times_h_squared
    ; r = pipe r
    ; z_out = pipe z_out
    ; s1 = pipe s1
    ; h_cubed = pipe h_cubed
    ; error = pipe error
    ; valid = pipe valid
    }
    |> map2 port_names ~f:(Fn.flip (Scope.naming scope))
  ;;
end

module Stage8 = struct
  type 'a t =
    { x_out : 'a
    ; u1_times_h_squared : 'a
    ; z_out : 'a
    ; h_cubed : 'a
    ; s1 : 'a
    ; r : 'a
    ; error : 'a
    ; valid : 'a
    }
  [@@deriving sexp_of, hardcaml]

  let latency (_config : Config.t) =
    Modulo_subtractor_pipe.latency ~stages:subtractor_stages
  ;;

  let create
    ~scope
    ~clock
    ~enable
    (config : Config.t)
    { Stage7.tmp0; tmp1; r; u1_times_h_squared; z_out; s1; h_cubed; error; valid }
    =
    let scope = Scope.sub_scope scope "stage8" in
    let spec = Reg_spec.create ~clock () in
    let pipe = pipeline spec ~n:(latency config) ~enable in
    let sub_pipe = sub_pipe ~latency ~config ~enable ~clock in
    { x_out = sub_pipe tmp0 tmp1
    ; u1_times_h_squared = pipe u1_times_h_squared
    ; z_out = pipe z_out
    ; s1 = pipe s1
    ; r = pipe r
    ; h_cubed = pipe h_cubed
    ; error = pipe error
    ; valid = pipe valid
    }
    |> map2 port_names ~f:(Fn.flip (Scope.naming scope))
  ;;
end

module Stage9 = struct
  type 'a t =
    { x_out : 'a
    ; tmp2 : 'a
    ; z_out : 'a
    ; h_cubed : 'a
    ; s1 : 'a
    ; r : 'a
    ; error : 'a
    ; valid : 'a
    }
  [@@deriving sexp_of, hardcaml]

  let latency (_config : Config.t) =
    Modulo_subtractor_pipe.latency ~stages:subtractor_stages
  ;;

  let create
    ~scope
    ~clock
    ~enable
    (config : Config.t)
    { Stage8.x_out; r; u1_times_h_squared; z_out; s1; h_cubed; error; valid }
    =
    let scope = Scope.sub_scope scope "stage9" in
    let spec = Reg_spec.create ~clock () in
    let pipe = pipeline spec ~n:(latency config) ~enable in
    let sub_pipe = sub_pipe ~latency ~config ~enable ~clock in
    { tmp2 = sub_pipe u1_times_h_squared x_out
    ; x_out = pipe x_out
    ; z_out = pipe z_out
    ; s1 = pipe s1
    ; r = pipe r
    ; h_cubed = pipe h_cubed
    ; error = pipe error
    ; valid = pipe valid
    }
    |> map2 port_names ~f:(Fn.flip (Scope.naming scope))
  ;;
end

module Stage10 = struct
  type 'a t =
    { r_times_tmp2 : 'a
    ; s1_times_h_cubed : 'a
    ; x_out : 'a
    ; z_out : 'a
    ; error : 'a
    ; valid : 'a
    }
  [@@deriving sexp_of, hardcaml]

  let latency_without_arbitration (config : Config.t) =
    Config.multiply_latency ~reduce:true config
  ;;

  let latency (config : Config.t) = latency_without_arbitration config + 1

  let create
    ~scope
    ~clock
    ~enable
    (config : Config.t)
    { Stage9.tmp2; x_out; z_out; s1; r; h_cubed; error; valid }
    =
    let scope = Scope.sub_scope scope "stage10" in
    let spec = Reg_spec.create ~clock () in
    let pipe = pipeline spec ~n:(latency config) ~enable in
    let r_times_tmp2, s1_times_h_cubed =
      arbitrate_multiply
        ~config
        ~scope
        ~clock
        ~enable
        ~valid
        ~latency_without_arbitration
        (r, tmp2)
        (s1, h_cubed)
    in
    { r_times_tmp2
    ; s1_times_h_cubed
    ; x_out = pipe x_out
    ; z_out = pipe z_out
    ; error = pipe error
    ; valid = pipe valid
    }
    |> map2 port_names ~f:(Fn.flip (Scope.naming scope))
  ;;
end

module Stage11 = struct
  type 'a t =
    { data_out : 'a Jacobian.t
    ; error : 'a
    ; valid_out : 'a
    }
  [@@deriving sexp_of, hardcaml]

  let latency (_ : Config.t) = Modulo_subtractor_pipe.latency ~stages:subtractor_stages

  let create
    ~scope
    ~clock
    ~enable
    (config : Config.t)
    { Stage10.r_times_tmp2; s1_times_h_cubed; x_out; z_out; error; valid }
    =
    let scope = Scope.sub_scope scope "stage11" in
    let spec = Reg_spec.create ~clock () in
    let sub_pipe = sub_pipe ~latency ~config ~enable ~clock in
    let pipe = pipeline spec ~n:(latency config) ~enable in
    { data_out =
        { x = pipe x_out; y = sub_pipe r_times_tmp2 s1_times_h_cubed; z = pipe z_out }
    ; error = pipe error
    ; valid_out = pipe valid
    }
    |> map2 port_names ~f:(Fn.flip (Scope.naming scope))
  ;;
end

let latency config =
  Stage1.latency config
  + Stage2.latency config
  + Stage3.latency config
  + Stage4.latency config
  + Stage5.latency config
  + Stage6.latency config
  + Stage7.latency config
  + Stage8.latency config
  + Stage9.latency config
  + Stage10.latency config
  + Stage11.latency config
;;

module With_interface (M : sig
  val bits : int
end) =
struct
  include M

  let latency = latency

  module Jacobian = struct
    type 'a t = 'a Jacobian.t =
      { x : 'a [@bits bits]
      ; y : 'a [@bits bits]
      ; z : 'a [@bits bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Affine = struct
    type 'a t = 'a Affine.t =
      { x : 'a [@bits bits]
      ; y : 'a [@bits bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; enable : 'a
      ; valid_in : 'a [@rtlprefix "valid_in_"]
      ; data_in0 : 'a Affine.t [@rtlprefix "data_in0_"]
      ; data_in1 : 'a Jacobian.t [@rtlprefix "data_in1_"]
      ; data_in1_z_squared : 'a [@bits bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { valid_out : 'a [@rtlprefix "out_"]
      ; ready_in : 'a
      ; data_out : 'a Jacobian.t [@rtlprefix "out_"]
      ; error : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create
    ~(config : Config.t)
    (scope : Scope.t)
    { I.clock; enable; valid_in; data_in0; data_in1; data_in1_z_squared }
    =
    let ready_in =
      Signal.reg_fb (Reg_spec.create ~clock ()) ~width:1 ~f:(fun fb ->
        mux2 (fb ==:. 0) vdd ~:valid_in)
    in
    let { Stage11.valid_out; data_out; error } =
      Stage0.name
        scope
        { valid_in = valid_in &: ready_in; data_in0; data_in1; data_in1_z_squared }
      |> Stage1.create ~clock ~enable ~scope config
      |> Stage2.create ~clock ~enable ~scope config
      |> Stage3.create ~clock ~enable ~scope config
      |> Stage4.create ~clock ~enable ~scope config
      |> Stage5.create ~clock ~enable ~scope config
      |> Stage6.create ~clock ~enable ~scope config
      |> Stage7.create ~clock ~enable ~scope config
      |> Stage8.create ~clock ~enable ~scope config
      |> Stage9.create ~clock ~enable ~scope config
      |> Stage10.create ~clock ~enable ~scope config
      |> Stage11.create ~clock ~enable ~scope config
    in
    { O.data_out; valid_out; error; ready_in }
  ;;
end
