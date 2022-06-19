open! Base
open! Hardcaml
open! Signal
open! Reg_with_enable

module Stage0 = struct
  type 'a t =
    { x : 'a
    ; y : 'a
    ; valid : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module Config = struct
  type t =
    { multiplier_config : Karatsuba_ofman_mult.Config.t
    ; montgomery_reduction_config : Montgomery_reduction.Config.t
    }

  let latency ({ multiplier_config; montgomery_reduction_config } : t) =
    Karatsuba_ofman_mult.Config.latency multiplier_config
    + Montgomery_reduction.Config.latency montgomery_reduction_config
  ;;
end

let create
    ~(config : Config.t)
    ~scope
    ~clock
    ~enable
    ~(p : Z.t)
    (x : Signal.t)
    (y : Signal.t)
  =
  assert (Signal.width x = Signal.width y);
  let xy =
    Karatsuba_ofman_mult.hierarchical
      ~enable
      ~config:config.multiplier_config
      ~scope
      ~clock
      x
      (`Signal y)
  in
  Montgomery_reduction.hierarchical
    ~scope
    ~config:config.montgomery_reduction_config
    ~clock
    ~enable
    ~p
    xy
;;

module With_interface (M : sig
  val bits : int
end) =
struct
  module Config = Config
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

  let create ~(config : Config.t) ~p scope { I.clock; enable; x; y; valid } =
    let spec = Reg_spec.create ~clock () in
    let result = create ~scope ~config ~clock ~enable ~p x y in
    let valid = pipeline spec ~enable ~n:(Config.latency config) valid in
    { O.z = result; valid }
  ;;
end
