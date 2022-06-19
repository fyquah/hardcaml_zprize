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
open! Reg_with_enable

let subtractor_stages = 3
let adder_stages = 3

module Config = struct
  type fn =
    { latency : int
    ; impl : scope:Scope.t -> clock:t -> enable:t -> t -> t option -> t
    }

  type t =
    { fp_multiply : fn
    ; fp_square : fn
    ; p : Z.t
    }
end

let double_pipe ~scope ~latency ~(config : Config.t) ~clock ~enable a =
  let stages = adder_stages in
  let spec = Reg_spec.create ~clock () in
  a
  |> Modulo_double_pipe.hierarchical ~scope ~stages ~p:config.p ~clock ~enable
  |> pipeline spec ~enable ~n:(latency config - Modulo_double_pipe.latency ~stages)
;;

let triple_pipe ~scope ~latency ~(config : Config.t) ~clock ~enable a =
  let stages = adder_stages in
  let spec = Reg_spec.create ~clock () in
  a
  |> Modulo_triple_pipe.hierarchical ~scope ~stages ~p:config.p ~clock ~enable
  |> pipeline spec ~enable ~n:(latency config - Modulo_triple_pipe.latency ~stages)
;;

let fourfold_pipe ~scope ~latency ~(config : Config.t) ~clock ~enable a =
  let stages = adder_stages in
  let spec = Reg_spec.create ~clock () in
  a
  |> Modulo_fourfold_pipe.hierarchical ~scope ~stages ~p:config.p ~clock ~enable
  |> pipeline spec ~enable ~n:(latency config - Modulo_fourfold_pipe.latency ~stages)
;;

let sub_pipe ~latency ~(config : Config.t) ~clock ~enable a b =
  let spec = Reg_spec.create ~clock () in
  let stages = subtractor_stages in
  Modulo_subtractor_pipe.create ~clock ~enable ~stages:subtractor_stages ~p:config.p a b
  |> pipeline spec ~n:(latency config - Modulo_subtractor_pipe.latency ~stages) ~enable
;;

let fp_multiply ~latency ~(config : Config.t) ~scope ~clock ~enable subscope a b =
  let spec = Reg_spec.create ~clock () in
  let scope = Scope.sub_scope scope subscope in
  config.fp_multiply.impl ~scope ~clock ~enable a (Some b)
  |> pipeline spec ~enable ~n:(latency config - config.fp_multiply.latency)
;;

let fp_square ~latency ~(config : Config.t) ~scope ~clock ~enable subscope a =
  let spec = Reg_spec.create ~clock () in
  let scope = Scope.sub_scope scope subscope in
  config.fp_square.impl ~scope ~clock ~enable a None
  |> pipeline spec ~enable ~n:(latency config - config.fp_square.latency)
;;

module Jacobian = struct
  type 'a t =
    { x : 'a
    ; y : 'a
    ; z : 'a
    }
  [@@deriving sexp_of, hardcaml]
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

module Stage1 = struct
  type 'a t =
    { y_squared : 'a
    ; x_squared : 'a
    ; y_times_z : 'a
    ; x_times_4 : 'a
    ; valid : 'a
    }
  [@@deriving sexp_of, hardcaml]

  let latency (config : Config.t) =
    config.fp_multiply.latency
    |> Int.max (Modulo_fourfold_pipe.latency ~stages:adder_stages)
    |> Int.max config.fp_square.latency
  ;;

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
    let fp_multiply = fp_multiply ~latency ~config ~scope ~clock ~enable in
    let fp_square = fp_square ~latency ~config ~scope ~clock ~enable in
    let fourfold_pipe = fourfold_pipe ~scope ~latency ~config ~clock ~enable in
    { y_squared = fp_square "y_squared" y
    ; x_squared = fp_square "x_squared" x
    ; y_times_z = fp_multiply "y_times_z" y z
    ; x_times_4 = fourfold_pipe x
    ; valid = pipe valid_in
    }
    |> map2 port_names ~f:(Fn.flip (Scope.naming scope))
  ;;
end

module Stage2 = struct
  type 'a t =
    { y_pow_4 : 'a
    ; m : 'a
    ; s : 'a
    ; z' : 'a
    ; valid : 'a
    }
  [@@deriving sexp_of, hardcaml]

  let latency (config : Config.t) =
    config.fp_multiply.latency
    |> Int.max config.fp_square.latency
    |> Int.max (Modulo_triple_pipe.latency ~stages:adder_stages)
  ;;

  let create
      ~scope
      ~clock
      ~enable
      (config : Config.t)
      { Stage1.y_squared; x_squared; x_times_4; y_times_z; valid }
    =
    let spec = Reg_spec.create ~clock () in
    let pipe = pipeline spec ~n:(latency config) ~enable in
    let fp_multiply = fp_multiply ~latency ~config ~scope ~clock ~enable in
    let fp_square = fp_square ~latency ~config ~scope ~clock ~enable in
    let triple_pipe = triple_pipe ~scope ~latency ~config ~clock ~enable in
    let double_pipe = double_pipe ~scope ~latency ~config ~clock ~enable in
    { y_pow_4 = fp_square "y_pow_4" y_squared
    ; m = triple_pipe x_squared
    ; s = fp_multiply "s" x_times_4 y_squared
    ; z' = double_pipe y_times_z
    ; valid = pipe valid
    }
    |> map2 port_names ~f:(Fn.flip (Scope.naming (Scope.sub_scope scope "stage2")))
  ;;
end

module Stage3 = struct
  type 'a t =
    { y_pow_4_times_2 : 'a
    ; s_times_2 : 'a
    ; m_pow_2 : 'a
    ; m : 'a
    ; s : 'a
    ; z' : 'a
    ; valid : 'a
    }
  [@@deriving sexp_of, hardcaml]

  let latency (config : Config.t) =
    config.fp_multiply.latency
    |> Int.max config.fp_square.latency
    |> Int.max (Modulo_double_pipe.latency ~stages:adder_stages)
  ;;

  let create ~scope ~clock ~enable (config : Config.t) { Stage2.y_pow_4; m; s; z'; valid }
    =
    let spec = Reg_spec.create ~clock () in
    let pipe = pipeline ~n:(latency config) ~enable spec in
    let fp_square = fp_square ~latency ~config ~scope ~clock ~enable in
    let double_pipe = double_pipe ~scope ~latency ~config ~clock ~enable in
    { y_pow_4_times_2 = double_pipe y_pow_4
    ; s_times_2 = double_pipe s
    ; m_pow_2 = fp_square "m_pow_2" m
    ; m = pipe m
    ; s = pipe s
    ; z' = pipe z'
    ; valid = pipe valid
    }
    |> map2 port_names ~f:(Fn.flip (Scope.naming (Scope.sub_scope scope "stage3")))
  ;;
end

module Stage4 = struct
  type 'a t =
    { x' : 'a
    ; z' : 'a
    ; m : 'a
    ; y_pow_4_times_4 : 'a
    ; s : 'a
    ; valid : 'a
    }
  [@@deriving sexp_of, hardcaml]

  let latency (_ : Config.t) =
    Int.max
      (Modulo_subtractor_pipe.latency ~stages:subtractor_stages)
      (Modulo_double_pipe.latency ~stages:adder_stages)
  ;;

  let create
      ~scope
      ~clock
      ~enable
      (config : Config.t)
      { Stage3.y_pow_4_times_2; s_times_2; m_pow_2; s; z'; m; valid }
    =
    let spec = Reg_spec.create ~clock () in
    let pipe = pipeline ~n:(latency config) ~enable spec in
    let sub_pipe = sub_pipe ~latency ~config ~enable ~clock in
    let double_pipe = double_pipe ~scope ~latency ~config ~clock ~enable in
    { x' = sub_pipe m_pow_2 s_times_2
    ; m = pipe m
    ; s = pipe s
    ; z' = pipe z'
    ; y_pow_4_times_4 = double_pipe y_pow_4_times_2
    ; valid = pipe valid
    }
    |> map2 port_names ~f:(Fn.flip (Scope.naming (Scope.sub_scope scope "stage4")))
  ;;
end

module Stage5 = struct
  type 'a t =
    { m : 'a
    ; s_minus_x' : 'a
    ; y_pow_4_times_8 : 'a
    ; x' : 'a
    ; z' : 'a
    ; valid : 'a
    }
  [@@deriving sexp_of, hardcaml]

  let latency (_ : Config.t) =
    Int.max
      (Modulo_subtractor_pipe.latency ~stages:subtractor_stages)
      (Modulo_double_pipe.latency ~stages:adder_stages)
  ;;

  let create
      ~scope
      ~clock
      ~enable
      (config : Config.t)
      { Stage4.s; x'; z'; m; y_pow_4_times_4; valid }
    =
    let spec = Reg_spec.create ~clock () in
    let pipe = pipeline spec ~n:(latency config) ~enable in
    let sub_pipe = sub_pipe ~latency ~config ~clock ~enable in
    let double_pipe = double_pipe ~scope ~latency ~config ~clock ~enable in
    { s_minus_x' = sub_pipe s x'
    ; m = pipe m
    ; y_pow_4_times_8 = double_pipe y_pow_4_times_4
    ; x' = pipe x'
    ; z' = pipe z'
    ; valid = pipe valid
    }
    |> map2 port_names ~f:(Fn.flip (Scope.naming (Scope.sub_scope scope "stage5")))
  ;;
end

module Stage6 = struct
  type 'a t =
    { m_times_s_minus_x' : 'a
    ; y_pow_4_times_8 : 'a
    ; x' : 'a
    ; z' : 'a
    ; valid : 'a
    }
  [@@deriving sexp_of, hardcaml]

  let latency (config : Config.t) = config.fp_multiply.latency

  let create
      ~scope
      ~clock
      ~enable
      (config : Config.t)
      { Stage5.s_minus_x'; m; y_pow_4_times_8; x'; z'; valid }
    =
    let scope = Scope.sub_scope scope "stage6" in
    let spec = Reg_spec.create ~clock () in
    let pipe = pipeline spec ~n:(latency config) ~enable in
    let fp_multiply = fp_multiply ~latency ~config ~scope ~clock ~enable in
    { m_times_s_minus_x' = fp_multiply "m_times_s_minus_x'" m s_minus_x'
    ; y_pow_4_times_8 = pipe y_pow_4_times_8
    ; x' = pipe x'
    ; z' = pipe z'
    ; valid = pipe valid
    }
    |> map2 port_names ~f:(Fn.flip (Scope.naming scope))
  ;;
end

module Stage7 = struct
  type 'a t =
    { valid_out : 'a [@rtlname "valid"]
    ; data_out : 'a Jacobian.t
    }
  [@@deriving sexp_of, hardcaml]

  let latency (_ : Config.t) = Modulo_subtractor_pipe.latency ~stages:subtractor_stages

  let create
      ~scope
      ~clock
      ~enable
      (config : Config.t)
      { Stage6.m_times_s_minus_x'; y_pow_4_times_8; x'; z'; valid }
      : _ t
    =
    let spec = Reg_spec.create ~clock () in
    let pipe = pipeline spec ~n:(latency config) ~enable in
    { data_out =
        { x = pipe x'
        ; y =
            Modulo_subtractor_pipe.create
              ~clock
              ~enable
              ~stages:subtractor_stages
              ~p:config.p
              m_times_s_minus_x'
              y_pow_4_times_8
        ; z = pipe z'
        }
    ; valid_out = pipe valid
    }
    |> map2 port_names ~f:(Fn.flip (Scope.naming (Scope.sub_scope scope "stage7")))
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
    type 'a t =
      { valid_out : 'a [@rtlprefix "out_"]
      ; data_out : 'a Jacobian.t [@rtlprefix "out_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create ~(config : Config.t) (scope : Scope.t) { I.clock; enable; valid_in; data_in }
    =
    let { Stage7.valid_out; data_out } =
      Stage0.name scope { valid_in; data_in }
      |> Stage1.create ~clock ~enable ~scope config
      |> Stage2.create ~clock ~enable ~scope config
      |> Stage3.create ~clock ~enable ~scope config
      |> Stage4.create ~clock ~enable ~scope config
      |> Stage5.create ~clock ~enable ~scope config
      |> Stage6.create ~clock ~enable ~scope config
      |> Stage7.create ~clock ~enable ~scope config
    in
    { O.data_out; valid_out }
  ;;
end
