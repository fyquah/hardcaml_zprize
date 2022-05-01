open! Base
open! Hardcaml
open! Signal

(* Shadow Signal.reg and Signal.pipeline because we want ~enable to be a
 * non-optional argument.
 *)
let reg spec ~enable x = Signal.reg spec ~enable x
let pipeline spec ~enable ~n x = Signal.pipeline spec ~enable ~n x

let _dont_warn_if_unused = (reg, pipeline)

module Stage0 = struct
  type 'a t =
    { x : 'a
    ; y : 'a
    ; valid : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module Stage1 = struct
  type 'a t =
    { xy : 'a
    ; valid : 'a
    }
  [@@deriving sexp_of, hardcaml]

  let create ~depth ~clock ~enable { Stage0. x; y; valid } =
    let latency = Karatsuba_ofman_mult.latency ~depth in
    { xy =
      Karatsuba_ofman_mult.create
        ~clock
        ~enable
        ~depth
        x
        y
    ; valid = Signal.pipeline (Reg_spec.create ~clock ()) ~enable ~n:latency valid
    }
  ;;
end

(* Computes m = [(xy mod r) * p' mod r], where P'P = -1 mod r and r is a power
 * of two.
 *)
module Stage2 = struct
  type 'a t =
    { m : 'a
    ; xy : 'a
    ; valid : 'a
    }
  [@@deriving sexp_of, hardcaml]

  let create ~depth ~(logr : int) ~p' ~clock ~enable { Stage1. xy; valid } =
    let spec = Reg_spec.create ~clock () in
    let m =
      Karatsuba_ofman_mult.create
        ~clock
        ~enable
        ~depth
        (sel_bottom xy logr)
        (of_z ~width:logr p')
      |> Fn.flip sel_bottom logr
    in
    let latency = Karatsuba_ofman_mult.latency ~depth in
    { m
    ; xy = pipeline spec ~n:latency ~enable xy
    ; valid = pipeline (Reg_spec.create ~clock ()) ~enable ~n:latency valid
    }
  ;;
end

module Stage3 = struct
  type 'a t =
    { mp : 'a
    ; xy : 'a
    ; valid : 'a
    }
  [@@deriving sexp_of, hardcaml]

  let create ~depth ~p ~clock ~enable { Stage2. m; xy; valid } =
    let spec = Reg_spec.create ~clock () in
    let mp =
      let width = width m in
      Karatsuba_ofman_mult.create
        ~clock
        ~enable
        ~depth
        m
        (of_z ~width p)
    in
    let latency = (Karatsuba_ofman_mult.latency ~depth) in
    { mp
    ; xy = pipeline spec ~enable ~n:latency xy
    ; valid = pipeline (Reg_spec.create ~clock ()) ~enable ~n:latency valid
    }
  ;;
end

module Stage4 = struct
  type 'a t =
    { t : 'a
    ; valid : 'a
    }
  [@@deriving sexp_of, hardcaml]

  let create ~depth ~logr ~clock ~enable { Stage3. mp; xy; valid } =
    assert (Signal.width mp = Signal.width xy);
    let t =
      let width = width mp in
      Adder_pipe.create ~clock ~enable
        ~stages:depth
        ~p:(Signal.zero width)
        xy
        mp
      |> Fn.flip drop_bottom logr
    in
    let latency = Adder_pipe.latency ~stages:depth in
    { t
    ; valid = pipeline (Reg_spec.create ~clock ()) ~enable ~n:latency valid
    }
  ;;
end

module Stage5 = struct
  type 'a t =
    { result : 'a
    ; valid : 'a
    }
  [@@deriving sexp_of, hardcaml, fields]

  let create ~depth ~p ~clock ~enable { Stage4. t; valid } =
    let width = width t in
    let p = of_z ~width p in
    let latency = Subtracter_pipe.latency ~stages:depth in
    { result =
        Subtracter_pipe.create ~clock ~enable
          ~stages:depth
          ~p
          t
          p
    ; valid = pipeline (Reg_spec.create ~clock ()) ~enable ~n:latency valid
    }
  ;;
end

module Config = struct
  type t =
    { multiplier_depth : int
    ; adder_depth : int
    ; subtracter_depth : int
    }

  let latency ({ multiplier_depth; adder_depth; subtracter_depth } : t) =
    3 * (Karatsuba_ofman_mult.latency ~depth:multiplier_depth)
    + adder_depth
    + subtracter_depth
  ;;
end

let create
    ~(config : Config.t)
    ~scope
    ~clock
    ~enable
    ~(p : Z.t)
    ~valid
    (x : Signal.t)
    (y : Signal.t) =
  assert (Signal.width x = Signal.width y);
  let logr = Signal.width x in
  let r = Z.(one lsl logr) in
  let p' =
    (* We want to find p' such that pp' = −1 mod r
     *
     * First we find
     * ar + bp = 1 using euclidean extended algorithm
     * <-> -ar - bp = -1
     * -> -bp = -1 mod r
     *
     * if b is negative, we're done, if it's not, we can do a little trick:
     *
     * -bp = (-b+r)p mod r
     *
     *)
    let { Extended_euclidean. coef_x = _ ; coef_y; gcd } =
      Extended_euclidean.extended_euclidean ~x:r ~y:p
    in
    assert (Z.equal gcd Z.one);
    let p' = Z.neg coef_y in
    if Z.lt p' Z.zero then
      Z.(p' + r)
    else
      p'
  in
  let (--) = Scope.naming scope in
  assert Z.(equal ((p * p') mod r) (r - one));
  { x; y; valid }
  |> Stage1.create ~depth:config.multiplier_depth ~clock ~enable
  |> Stage1.map2 Stage1.port_names ~f:(fun port_name x ->
      x -- ("stage1$" ^ port_name))
  |> Stage2.create ~depth:config.multiplier_depth ~logr ~p' ~clock ~enable
  |> Stage2.map2 Stage2.port_names ~f:(fun port_name x ->
      x -- ("stage2$" ^ port_name))
  |> Stage3.create ~depth:config.multiplier_depth ~p ~clock ~enable
  |> Stage3.map2 Stage3.port_names ~f:(fun port_name x ->
      x -- ("stage3$" ^ port_name))
  |> Stage4.create ~depth:config.adder_depth ~logr ~clock ~enable
  |> Stage4.map2 Stage4.port_names ~f:(fun port_name x ->
      x -- ("stage4$" ^ port_name))
  |> Stage5.create ~depth:config.subtracter_depth ~p ~clock ~enable
  |> Stage5.map2 Stage5.port_names ~f:(fun port_name x ->
      x -- ("stage5$" ^ port_name))
;;

module With_interface(M : sig val bits : int end) = struct
  include M

  module I = struct
    type 'a t =
      { clock : 'a
      ; enable : 'a
      ; x : 'a [@bits bits]
      ; y : 'a [@bits bits]
      ; valid : 'a [@rtlprefix "in_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { z : 'a [@bits bits]
      ; valid : 'a [@rtlprefix "out_"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create
      ~(config : Config.t)
      ~p
      scope
      { I. clock; enable; x; y; valid } =
    let { Stage5. result; valid } =
      create ~valid ~scope ~config ~clock ~enable ~p x y 
    in
    { O. z = result ; valid }
  ;;
end
