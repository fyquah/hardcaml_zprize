open Base
open Hardcaml
open Field_ops_lib

module For_bls12_377 = struct
  let p = Ark_bls12_377_g1.modulus ()
  let montgomery_reduction_config = Montgomery_reduction.Config.for_bls12_377
  let barrett_reduction_config = Barrett_reduction.Config.for_bls12_377

  let barrett_coarse_reduction_config =
    { Barrett_reduction.Config.for_bls12_377 with include_fine_reduction = false }
  ;;

  let square : Ec_fpn_ops_config.fn =
    let config =
      { Squarer.Config.levels =
          [ { radix = Radix_3
            ; pre_adder_stages = 1
            ; middle_adder_stages = 1
            ; post_adder_stages = 1
            }
          ; { radix = Radix_3
            ; pre_adder_stages = 1
            ; middle_adder_stages = 1
            ; post_adder_stages = 1
            }
          ; { radix = Radix_2
            ; pre_adder_stages = 1
            ; middle_adder_stages = 1
            ; post_adder_stages = 1
            }
          ]
      ; ground_multiplier =
          Hybrid_dsp_and_luts { latency = 3; lut_only_hamming_weight_threshold = 6 }
      }
    in
    let latency = Squarer.Config.latency config in
    let impl ~scope ~clock ~enable x y =
      assert (Option.is_none y);
      Squarer.hierarchical ~config ~clock ~enable ~scope x
    in
    { impl; latency }
  ;;

  let multiply : Ec_fpn_ops_config.fn =
    let config =
      match Which_config.t with
      | Heavy_pipelining ->
        Karatsuba_ofman_mult.Config.generate
          [ { radix = Radix_2
            ; pre_adder_stages = 2
            ; middle_adder_stages = 4
            ; post_adder_stages = 10
            }
          ; { radix = Radix_2
            ; pre_adder_stages = 1
            ; middle_adder_stages = 2
            ; post_adder_stages = 5
            }
          ; { radix = Radix_2
            ; pre_adder_stages = 1
            ; middle_adder_stages = 1
            ; post_adder_stages = 1
            }
          ; { radix = Radix_2
            ; pre_adder_stages = 1
            ; middle_adder_stages = 0
            ; post_adder_stages = 1
            }
          ]
          ~ground_multiplier:(Verilog_multiply { latency = 2 })
      | Medium_pipelining ->
        Karatsuba_ofman_mult.Config.generate
          [ { radix = Radix_2
            ; pre_adder_stages = 2
            ; middle_adder_stages = 3
            ; post_adder_stages = 6
            }
          ; { radix = Radix_2
            ; pre_adder_stages = 1
            ; middle_adder_stages = 2
            ; post_adder_stages = 3
            }
          ; { radix = Radix_2
            ; pre_adder_stages = 1
            ; middle_adder_stages = 0
            ; post_adder_stages = 1
            }
          ; { radix = Radix_2
            ; pre_adder_stages = 1
            ; middle_adder_stages = 0
            ; post_adder_stages = 1
            }
          ]
          ~ground_multiplier:(Verilog_multiply { latency = 2 })
    in
    let latency = Karatsuba_ofman_mult.Config.latency config in
    let impl ~scope ~clock ~enable x y =
      Karatsuba_ofman_mult.hierarchical
        ~enable
        ~config
        ~scope
        ~clock
        x
        (`Signal (Option.value_exn y))
    in
    { latency; impl }
  ;;

  let montgomery_reduce : Ec_fpn_ops_config.fn =
    let config = montgomery_reduction_config in
    let latency = Montgomery_reduction.Config.latency config in
    let impl ~scope ~clock ~enable x y =
      assert (Option.is_none y);
      Montgomery_reduction.hierarchical ~config ~p ~scope ~clock ~enable x
    in
    { impl; latency }
  ;;

  let barrett_reduce_base ~coarse : Ec_fpn_ops_config.fn =
    let config =
      if coarse then barrett_coarse_reduction_config else barrett_reduction_config
    in
    let impl ~scope ~clock ~enable mult_value y =
      assert (Option.is_none y);
      let { With_valid.valid = _; value } =
        Barrett_reduction.hierarchical
          ~scope
          ~p
          ~clock
          ~enable
          ~config
          { valid = Signal.vdd; value = mult_value }
      in
      value
    in
    let latency = Barrett_reduction.Config.latency config in
    { impl; latency }
  ;;

  let barrett_reduce = barrett_reduce_base ~coarse:false
  let barrett_reduce_coarse = barrett_reduce_base ~coarse:true

  let ec_fpn_ops_with_montgomery_reduction =
    let reduce = montgomery_reduce in
    let coarse_reduce = montgomery_reduce in
    { Ec_fpn_ops_config.multiply; square; reduce; coarse_reduce; p }
  ;;

  let ec_fpn_ops_with_barrett_reduction =
    let reduce = barrett_reduce in
    let coarse_reduce = barrett_reduce_coarse in
    { Ec_fpn_ops_config.multiply; square; reduce; coarse_reduce; p }
  ;;
end
