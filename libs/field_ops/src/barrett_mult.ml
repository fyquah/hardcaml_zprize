open Base

module Config = struct
  type t =
    { multiplier_config : Karatsuba_ofman_mult.Config.t
    ; barrett_reduction_config : Barrett_reduction.Config.t
    }
end

module With_interface (M : sig
  val bits : int
end) =
struct
  open M
  module Karatsuba_ofman_mult = Karatsuba_ofman_mult.With_interface (M)

  module Barrett_reduction = Barrett_reduction.With_interface (struct
    include M

    let output_bits = bits
  end)

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

  let create ~(config : Config.t) ~p scope { I.clock; enable; x; y; valid = valid_in } =
    let mult_stage =
      Karatsuba_ofman_mult.hierarchical
        ~config:config.multiplier_config
        scope
        { clock; enable; valid = valid_in; a = x; b = y }
    in
    let barrett_reduction =
      Barrett_reduction.hierarchical
        ~config:config.barrett_reduction_config
        ~p
        scope
        { clock; enable; a = mult_stage.c; valid = mult_stage.valid }
    in
    { O.z = barrett_reduction.a_mod_p; valid = barrett_reduction.valid }
  ;;
end
