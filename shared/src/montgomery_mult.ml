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
    { multiplier_config :
        [ `Squarer of Squarer.Config.t | `Multiplier of Karatsuba_ofman_mult.Config.t ]
    ; montgomery_reduction_config : Montgomery_reduction.Config.t
    }

  let latency ({ multiplier_config; montgomery_reduction_config } : t) =
    (match multiplier_config with
    | `Multiplier multiplier_config ->
      Karatsuba_ofman_mult.Config.latency multiplier_config
    | `Squarer squarer_config -> Squarer.Config.latency squarer_config)
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
    (y : Signal.t option)
  =
  let xy =
    match config.multiplier_config with
    | `Multiplier config ->
      let y = Option.value_exn y in
      assert (Signal.width x = Signal.width y);
      Karatsuba_ofman_mult.hierarchical ~enable ~config ~scope ~clock x (`Signal y)
    | `Squarer config ->
      assert (Option.is_none y);
      Squarer.hierarchical ~config ~scope ~clock ~enable x
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
    let result =
      create
        ~scope
        ~config
        ~clock
        ~enable
        ~p
        x
        (match config.multiplier_config with
        | `Multiplier _ -> Some y
        | `Squarer _ -> None)
    in
    let valid = pipeline spec ~enable ~n:(Config.latency config) valid in
    { O.z = result; valid }
  ;;
end
