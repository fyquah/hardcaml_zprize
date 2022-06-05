(* Calculates [a mod p] using barret reduction, where [p] is a compile-time
   known constant.
  
   Based on implementation in 
   https://github.com/ZcashFoundation/zcash-fpga/blob/c4c0ad918898084c73528ca231d025e36740d40c/ip_cores/util/src/rtl/barret_mod_pipe.sv
  
   Computes the following in separate pipeline stages, where
   k = Int.ceillog(p)
   
   {[
     let reduce ~p ~k a =
       let q = (a * m) >> k in
       let qp = q * p in
       let a_minus_qp = a - qp in
       mux2 (a_minus_qp >= p)
         (a_minus_qp - p)
         a_minus_qp
   ]}
 *)

open Base
open Hardcaml
open Signal

(* Shadow Signal.reg and Signal.pipeline because we want ~enable to be a
 * non-optional argument.
 *)
let reg spec ~enable x = Signal.reg spec ~enable x
let pipeline spec ~enable ~n x = Signal.pipeline spec ~enable ~n x

let _dont_warn_if_unused = (reg, pipeline)

module Config = struct
  type t =
    { multiplier_depth : int
    ; subtracter_stages : int
    }

  let latency (config : t) =
    (2 * Karatsuba_ofman_mult.latency ~depth:config.multiplier_depth)
    + (2 * Subtracter_pipe.latency ~stages:config.subtracter_stages)
  ;;
end

module Stage0 = struct
  type 'a t =
    { a : 'a
    ; valid : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module Stage1 = struct
  type 'a t =
    { q : 'a
    ; a : 'a
    ; valid : 'a
    }
  [@@deriving sexp_of, hardcaml]

  let create ~scope ~clock ~enable ~m ~k ~(config : Config.t) { Stage0. a; valid } =
    let logm = Z.log2up m in
    let wa = Signal.width a in
    let depth = config.multiplier_depth in
    let latency = Karatsuba_ofman_mult.latency ~depth in
    let spec = Reg_spec.create ~clock () in
    (* [a] can occupy up to [wa] bits whereas [m] and occupy [logm] bits, hence
       [a * m] can occupy [wa + logm] bits, and shifting by [k] should only occupy
       [wa + logm - k] bits
    *)
    { q = 
        Karatsuba_ofman_mult.create
          ~scope
          ~depth
          ~clock
          ~enable
          a
          (of_z ~width:(width a) m)
        |> Fn.flip drop_bottom k
        |> Fn.flip sel_bottom (wa + logm - k)
    ; a = pipeline ~enable ~n:latency spec a
    ; valid = pipeline ~enable ~n:latency spec valid
    }
  ;;
end

module Stage2 = struct
  type 'a t =
    { qp : 'a
    ; a : 'a
    ; valid : 'a
    }
  [@@deriving sexp_of, hardcaml]

  let create ~clock ~scope ~enable ~p ~(config : Config.t) { Stage1. q; a; valid } =
    let depth = config.multiplier_depth in
    let latency = Karatsuba_ofman_mult.latency ~depth in
    let spec = Reg_spec.create ~clock () in
    assert (width q >= Z.log2up p);
    { qp =
        Karatsuba_ofman_mult.create
          ~scope
          ~depth
          ~clock
          ~enable
          q
          (of_z ~width:(width q) p)
        |> Fn.flip sel_bottom (width a)
    ; a =
        pipeline
          ~enable
          ~n:latency
          spec
          a
    ; valid = pipeline ~enable ~n:latency spec valid
    }
  ;;
end

module Stage3 = struct
  type 'a t =
    { a_minus_qp : 'a
    ; valid : 'a
    }
  [@@deriving sexp_of, hardcaml]
    
  let create ~clock ~enable ~p ~(config : Config.t) { Stage2. qp; a; valid } =
    let spec = Reg_spec.create ~clock () in
    let stages = config.subtracter_stages in
    let latency = Subtracter_pipe.latency ~stages in
    let a_minus_qp_width =
      (* [a - qp] possible range of values ios [0, 2p) *)
      1 + (Z.log2up p)
    in
    { a_minus_qp =
        Subtracter_pipe.create
          ~clock
          ~enable
          ~stages
          ~p:(of_z ~width:(Signal.width a) p)
          a
          qp
        |> Fn.flip sel_bottom a_minus_qp_width
    ; valid = pipeline ~enable ~n:latency spec valid
    }
  ;;
end

module Stage4 = struct
  type 'a t =
    { a_mod_p : 'a
    ; valid : 'a
    }
  [@@deriving sexp_of, hardcaml]

  let create ~clock ~enable ~p ~(config : Config.t) { Stage3. a_minus_qp; valid } =
    let spec = Reg_spec.create ~clock () in
    let stages = config.subtracter_stages in
    let latency = Subtracter_pipe.latency ~stages in
    let p = of_z ~width:(Signal.width a_minus_qp) p in
    { a_mod_p =
        Subtracter_pipe.create
          ~clock
          ~enable
          ~stages
          ~p
          a_minus_qp
          p
    ; valid = pipeline ~enable ~n:latency spec valid
    }
  ;;
end

module With_interface(M : sig val bits : int end) = struct
  include M

  module I = struct
    type 'a t =
      { clock : 'a
      ; enable : 'a
      ; a : 'a [@bits (2 * bits)]
      ; valid : 'a  [@rtlprefix "in_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { a_mod_p : 'a [@bits bits]
      ; valid : 'a [@rtlprefix "out_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create ~config ~p scope { I. clock; enable; a; valid } =
    (* TODO(fyquah): Not sure if [m] or [k] is appropriate here... i made it
       up :)
    *)
    let k = 2 * Z.log2up p in
    let m = Z.((one lsl k) / p) in
    let { Stage4. a_mod_p; valid } =
      let (--) = Scope.naming scope in
      { Stage0. a; valid }
      |> Stage1.create ~scope ~clock ~enable ~m ~k ~config
      |> Stage1.map2 Stage1.port_names ~f:(fun n s ->
          s -- ("stage1$" ^ n))
      |> Stage2.create ~scope ~clock ~enable ~p ~config
      |> Stage2.map2 Stage2.port_names ~f:(fun n s ->
          s -- ("stage2$" ^ n))
      |> Stage3.create ~clock ~enable ~p ~config
      |> Stage3.map2 Stage3.port_names ~f:(fun n s ->
          s -- ("stage3$" ^ n))
      |> Stage4.create ~clock ~enable ~p ~config
      |> Stage4.map2 Stage4.port_names ~f:(fun n s ->
          s -- ("stage4$" ^ n))
    in
    { O. valid; a_mod_p = uresize a_mod_p bits }
  ;;
end
