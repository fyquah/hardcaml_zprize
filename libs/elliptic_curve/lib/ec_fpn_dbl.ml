(* Doubling is based on the formulae in
 * https://en.wikibooks.org/wiki/Cryptography/Prime_Curve/Jacobian_Coordinates
 *
 * This implements the section "Point Doubling", replacing multiplication
 * by constants with additions (adders are a lot cheaper than multipliers).
 * It is possible that implementing "Repeated Doubling" with a slightly
 * different I/O interface can save some adders. Revisit as necessary.
 *)

open! Base
open! Hardcaml
open! Signal
open! Field_ops_lib
open! Reg_with_enable
module Jacobian = Point.Jacobian

(* TODO(fyquah): Make [adder_stages] and [subtractor_stages] configurable. *)
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

let triple_pipe ~scope ~latency ~(config : Config.t) ~clock ~enable a =
  let stages = adder_stages in
  let spec = Reg_spec.create ~clock () in
  a
  |> Modulo_triple_pipe.hierarchical ~scope ~stages ~p:config.p ~clock ~enable
  |> pipeline spec ~enable ~n:(latency config - Modulo_triple_pipe.latency ~stages)
;;

let sub_pipe ~latency ~(config : Config.t) ~clock ~enable a b =
  let spec = Reg_spec.create ~clock () in
  let stages = subtractor_stages in
  Modulo_subtractor_pipe.create ~clock ~enable ~stages:subtractor_stages ~p:config.p a b
  |> pipeline spec ~n:(latency config - Modulo_subtractor_pipe.latency ~stages) ~enable
;;

module O_data = struct
  type 'a t =
    { point : 'a Jacobian.t
    ; z_squared : 'a
    }
end

module Stage0 = struct
  type 'a t =
    { valid_in : 'a [@rtlname "valid"]
    ; data_in : 'a Jacobian.t
    }
  [@@deriving sexp_of, hardcaml]

  let name scope =
    map2 port_names ~f:(Fn.flip (Scope.naming (Scope.sub_scope scope "stage0")))
  ;;
end

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

module Stage1 = struct
  type 'a t =
    { y_squared : 'a
    ; x_squared : 'a
    ; x : 'a
    ; y : 'a
    ; z : 'a
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
    { Stage0.data_in = { x; y; z }; valid_in }
    =
    let scope = Scope.sub_scope scope "stage1" in
    let spec = Reg_spec.create ~clock () in
    let pipe = pipeline spec ~n:(latency config) ~enable in
    let y_squared, x_squared =
      arbitrate_square
        ~config
        ~scope
        ~clock
        ~enable
        ~valid:valid_in
        ~latency_without_arbitration
        y
        x
    in
    { y_squared; x_squared; x = pipe x; y = pipe y; z = pipe z; valid = pipe valid_in }
    |> map2 port_names ~f:(Fn.flip (Scope.naming scope))
  ;;
end

module Stage2 = struct
  type 'a t =
    { y_squared_times_2 : 'a
    ; m : 'a
    ; x_times_2 : 'a
    ; y : 'a
    ; z : 'a
    ; valid : 'a
    }
  [@@deriving sexp_of, hardcaml]

  let latency _ = Modulo_triple_pipe.latency ~stages:adder_stages

  let create
    ~scope
    ~clock
    ~enable
    (config : Config.t)
    { Stage1.y_squared; x_squared; x; y; z; valid }
    =
    let scope = Scope.sub_scope scope "stage2" in
    let spec = Reg_spec.create ~clock () in
    let pipe = pipeline spec ~n:(latency config) ~enable in
    let double_pipe = double_pipe ~scope ~latency ~config ~clock ~enable in
    let triple_pipe = triple_pipe ~scope ~latency ~config ~clock ~enable in
    { y_squared_times_2 = double_pipe ~twice_width:false y_squared
    ; x_times_2 = double_pipe ~twice_width:false x
    ; m = triple_pipe x_squared
    ; y = pipe y
    ; z = pipe z
    ; valid = pipe valid
    }
    |> map2 port_names ~f:(Fn.flip (Scope.naming scope))
  ;;
end

module Stage3 = struct
  type 'a t =
    { y_pow_4_times_4 : 'a
    ; m : 'a
    ; m_squared : 'a
    ; y_times_z : 'a
    ; s : 'a
    ; valid : 'a
    }
  [@@deriving sexp_of, hardcaml]

  let latency_without_arbitration (config : Config.t) =
    Int.max
      (Config.multiply_latency ~reduce:true config)
      (Config.square_latency ~reduce:true config)
  ;;

  let latency (config : Config.t) = latency_without_arbitration config + 1

  let create
    ~scope
    ~clock
    ~enable
    (config : Config.t)
    { Stage2.y_squared_times_2; m; x_times_2; y; z; valid }
    =
    let scope = Scope.sub_scope scope "stage3" in
    let spec = Reg_spec.create ~clock () in
    let pipe = pipeline spec ~n:(latency config) ~enable in
    let y_pow_4_times_4, m_squared =
      arbitrate_square
        ~config
        ~scope
        ~clock
        ~enable
        ~valid
        ~latency_without_arbitration
        y_squared_times_2
        m
    in
    let y_times_z, s =
      arbitrate_multiply
        ~config
        ~scope
        ~clock
        ~enable
        ~valid
        ~latency_without_arbitration
        (y, z)
        (y_squared_times_2, x_times_2)
    in
    { y_pow_4_times_4; m_squared; y_times_z; s; m = pipe m; valid = pipe valid }
    |> map2 port_names ~f:(Fn.flip (Scope.naming scope))
  ;;
end

module Stage4 = struct
  type 'a t =
    { m_squared : 'a
    ; m : 'a
    ; s_times_2 : 'a
    ; z' : 'a
    ; s : 'a
    ; y_pow_4_times_8 : 'a
    ; valid : 'a
    }
  [@@deriving sexp_of, hardcaml]

  let latency _ = Modulo_double_pipe.latency ~stages:adder_stages

  let create
    ~scope
    ~clock
    ~enable
    (config : Config.t)
    { Stage3.y_pow_4_times_4; m_squared; y_times_z; s; m; valid }
    =
    let spec = Reg_spec.create ~clock () in
    let pipe = pipeline ~n:(latency config) ~enable spec in
    let double_pipe = double_pipe ~scope ~latency ~config ~clock ~enable in
    { m_squared = pipe m_squared
    ; m = pipe m
    ; s_times_2 = double_pipe ~twice_width:false s
    ; z' = double_pipe ~twice_width:false y_times_z
    ; y_pow_4_times_8 = double_pipe ~twice_width:false y_pow_4_times_4
    ; s = pipe s
    ; valid = pipe valid
    }
    |> map2 port_names ~f:(Fn.flip (Scope.naming (Scope.sub_scope scope "stage4")))
  ;;
end

module Stage5 = struct
  type 'a t =
    { x' : 'a
    ; z' : 'a
    ; s : 'a
    ; m : 'a
    ; y_pow_4_times_8 : 'a
    ; valid : 'a
    }
  [@@deriving sexp_of, hardcaml]

  let latency _ = Modulo_subtractor_pipe.latency ~stages:subtractor_stages

  let create
    ~scope
    ~clock
    ~enable
    (config : Config.t)
    { Stage4.m_squared; s_times_2; z'; y_pow_4_times_8; valid; s; m }
    =
    let spec = Reg_spec.create ~clock () in
    let pipe = pipeline ~n:(latency config) ~enable spec in
    let sub_pipe = sub_pipe ~latency ~config ~enable ~clock in
    { x' = sub_pipe m_squared s_times_2
    ; z' = pipe z'
    ; s = pipe s
    ; m = pipe m
    ; y_pow_4_times_8 = pipe y_pow_4_times_8
    ; valid = pipe valid
    }
    |> map2 port_names ~f:(Fn.flip (Scope.naming (Scope.sub_scope scope "stage5")))
  ;;
end

module Stage6 = struct
  type 'a t =
    { x' : 'a
    ; z' : 'a
    ; m : 'a
    ; s_minus_x' : 'a
    ; y_pow_4_times_8 : 'a
    ; valid : 'a
    }
  [@@deriving sexp_of, hardcaml]

  let latency _ = Modulo_subtractor_pipe.latency ~stages:subtractor_stages

  let create
    ~scope
    ~clock
    ~enable
    (config : Config.t)
    { Stage5.x'; z'; s; m; valid; y_pow_4_times_8 }
    =
    let spec = Reg_spec.create ~clock () in
    let pipe = pipeline ~n:(latency config) ~enable spec in
    let sub_pipe = sub_pipe ~latency ~config ~enable ~clock in
    { x' = pipe x'
    ; z' = pipe z'
    ; s_minus_x' = sub_pipe s x'
    ; m = pipe m
    ; y_pow_4_times_8 = pipe y_pow_4_times_8
    ; valid = pipe valid
    }
    |> map2 port_names ~f:(Fn.flip (Scope.naming (Scope.sub_scope scope "stage6")))
  ;;
end

module Stage7 = struct
  type 'a t =
    { x' : 'a
    ; z' : 'a
    ; m_times_s_minus_x' : 'a
    ; y_pow_4_times_8 : 'a
    ; z'_squared : 'a
    ; valid : 'a
    }

  let latency_without_arbitration (config : Config.t) =
    Config.multiply_latency ~reduce:true config
  ;;

  let latency (config : Config.t) = latency_without_arbitration config + 1

  let create
    ~scope
    ~clock
    ~enable
    (config : Config.t)
    { Stage6.x'; z'; m; s_minus_x'; y_pow_4_times_8; valid }
    =
    let spec = Reg_spec.create ~clock () in
    let pipe = pipeline ~n:(latency config) ~enable spec in
    let m_times_s_minus_x', z'_squared =
      arbitrate_multiply
        ~config
        ~scope
        ~clock
        ~enable
        ~valid
        ~latency_without_arbitration
        (m, s_minus_x')
        (z', z')
    in
    { x' = pipe x'
    ; z' = pipe z'
    ; m_times_s_minus_x'
    ; z'_squared
    ; y_pow_4_times_8 = pipe y_pow_4_times_8
    ; valid = pipe valid
    }
  ;;
end

module Stage8 = struct
  type 'a t =
    { data_out : 'a O_data.t
    ; valid_out : 'a
    }

  let latency _ = Modulo_subtractor_pipe.latency ~stages:subtractor_stages

  let create
    ~scope:_
    ~clock
    ~enable
    (config : Config.t)
    { Stage7.x'; z'; z'_squared; m_times_s_minus_x'; y_pow_4_times_8; valid }
    =
    let spec = Reg_spec.create ~clock () in
    let pipe = pipeline ~n:(latency config) ~enable spec in
    let sub_pipe = sub_pipe ~latency ~config ~enable ~clock in
    { data_out =
        { point =
            { x = pipe x'; y = sub_pipe m_times_s_minus_x' y_pow_4_times_8; z = pipe z' }
        ; z_squared = pipe z'_squared
        }
    ; valid_out = pipe valid
    }
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

  module I = struct
    type 'a t =
      { clock : 'a
      ; enable : 'a
      ; valid_in : 'a [@rtlprefix "in_"]
      ; data_in : 'a Jacobian.t [@rtlprefix "in_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    module Data = struct
      type 'a t = 'a O_data.t =
        { point : 'a Jacobian.t
        ; z_squared : 'a [@bits bits]
        }
      [@@deriving sexp_of, hardcaml]
    end

    type 'a t =
      { ready_in : 'a
      ; valid_out : 'a [@rtlprefix "out_"]
      ; data_out : 'a Data.t [@rtlprefix "out_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create ~(config : Config.t) (scope : Scope.t) { I.clock; enable; valid_in; data_in }
    =
    let ready_in =
      Signal.reg_fb (Reg_spec.create ~clock ()) ~width:1 ~f:(fun fb ->
        mux2 (fb ==:. 0) vdd ~:valid_in)
    in
    let { Stage8.valid_out; data_out } =
      Stage0.name scope { valid_in = valid_in &: ready_in; data_in }
      |> Stage1.create ~clock ~enable ~scope config
      |> Stage2.create ~clock ~enable ~scope config
      |> Stage3.create ~clock ~enable ~scope config
      |> Stage4.create ~clock ~enable ~scope config
      |> Stage5.create ~clock ~enable ~scope config
      |> Stage6.create ~clock ~enable ~scope config
      |> Stage7.create ~clock ~enable ~scope config
      |> Stage8.create ~clock ~enable ~scope config
    in
    { O.data_out; valid_out; ready_in }
  ;;
end
