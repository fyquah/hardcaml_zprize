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
open Reg_with_enable

module Config = struct
  type t =
    { multiplier_config : Karatsuba_ofman_mult.Config.t
    ; half_multiplier_config : Half_width_multiplier.Config.t
    ; subtracter_stages : int
    }

  let latency (config : t) =
    (1 * Karatsuba_ofman_mult.Config.latency config.multiplier_config)
    + (1 * Half_width_multiplier.Config.latency config.half_multiplier_config)
    + config.subtracter_stages
    + config.subtracter_stages
    + 1
  ;;
end

module With_interface (M : sig
  val bits : int
end) =
struct
  include M

  let k = 2 * bits

  module Config = Config

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
      ; a' : 'a
      ; valid : 'a
      }
    [@@deriving sexp_of, hardcaml]

    let create ~scope ~clock ~enable ~m ~(config : Config.t) { Stage0.a; valid } =
      assert (width a = bits * 2);
      let multiplier_config = config.multiplier_config in
      let latency = Karatsuba_ofman_mult.Config.latency multiplier_config in
      let spec = Reg_spec.create ~clock () in
      (* [a] can occupy up to [wa] bits whereas [m] and occupy [bits + 1] bits, hence
         [a * m] can occupy [wa + logm] bits, and shifting by [k] should only occupy
         [wa + logm - k] bits
      *)
      let q =
        Karatsuba_ofman_mult.hierarchical
          ~scope
          ~config:multiplier_config
          ~clock
          ~enable
          (uresize (sel_top a bits) (bits + 1))
          (`Constant m)
        |> Fn.flip drop_bottom bits
        |> Fn.flip sel_bottom bits
      in
      { q
      ; a' = pipeline ~enable ~n:latency spec (sel_bottom a (bits + 2))
      ; valid = pipeline ~enable ~n:latency spec valid
      }
    ;;
  end

  module Stage2 = struct
    type 'a t =
      { qp : 'a
      ; a' : 'a
      ; valid : 'a
      }
    [@@deriving sexp_of, hardcaml]

    let create ~clock ~scope ~enable ~p ~(config : Config.t) { Stage1.q; a'; valid } =
      let latency = Half_width_multiplier.Config.latency config.half_multiplier_config in
      let spec = Reg_spec.create ~clock () in
      assert (width q = bits);
      { qp =
          Half_width_multiplier.hierarchical
            ~scope
            ~clock
            ~enable
            ~config:config.half_multiplier_config
            (Multiply_by_constant (uresize q (bits + 2), Bits.of_z ~width:(bits + 2) p))
      ; a' = pipeline ~enable ~n:latency spec a'
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

    let create ~scope ~clock ~enable ~(config : Config.t) { Stage2.qp; a'; valid } =
      let spec = Reg_spec.create ~clock () in
      let stages = config.subtracter_stages in
      let latency = Modulo_subtractor_pipe.latency ~stages in
      assert (width a' = bits + 2);
      assert (width qp = bits + 2);
      { a_minus_qp =
          Adder_subtractor_pipe.hierarchical
            ~stages
            ~scope
            ~enable
            ~clock
            { lhs = a'; rhs_list = [ { op = `Sub; term = qp } ] }
          |> List.hd_exn
          |> Adder_subtractor_pipe.Single_op_output.result
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

    let with_valid_value { With_valid.valid = _; value } = value

    let create ~scope ~clock ~enable ~p ~(config : Config.t) { Stage3.a_minus_qp; valid } =
      assert (width a_minus_qp = bits + 2);
      let spec = Reg_spec.create ~clock () in
      let stages = config.subtracter_stages in
      let latency = Modulo_subtractor_pipe.latency ~stages + 1 in
      let a_mod_p =
        List.map (List.range 0 4) ~f:(fun i ->
            let result =
              Adder_subtractor_pipe.hierarchical
                ~stages
                ~scope
                ~enable
                ~clock
                { lhs = a_minus_qp
                ; rhs_list =
                    [ { op = `Sub; term = Signal.of_z ~width:(bits + 2) Z.(of_int i * p) }
                    ]
                }
              |> List.hd_exn
            in
            { With_valid.valid = ~:(Adder_subtractor_pipe.Single_op_output.carry result)
            ; value = Adder_subtractor_pipe.Single_op_output.result result
            })
        |> List.rev
        |> Signal.priority_select
        |> with_valid_value
        |> reg ~enable spec
      in
      { a_mod_p; valid = pipeline ~enable ~n:latency spec valid }
    ;;
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; enable : 'a
      ; a : 'a [@bits 2 * bits]
      ; valid : 'a [@rtlprefix "in_"]
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

  let create ~config ~p scope { I.clock; enable; a; valid } =
    assert (Z.log2up p <= bits);
    let m = Z.((one lsl k) / p) in
    let { Stage4.a_mod_p; valid } =
      let ( -- ) = Scope.naming scope in
      { Stage0.a; valid }
      |> Stage1.create ~scope ~clock ~enable ~m ~config
      |> Stage1.map2 Stage1.port_names ~f:(fun n s -> s -- ("stage1$" ^ n))
      |> Stage2.create ~scope ~clock ~enable ~p ~config
      |> Stage2.map2 Stage2.port_names ~f:(fun n s -> s -- ("stage2$" ^ n))
      |> Stage3.create ~scope ~clock ~enable ~config
      |> Stage3.map2 Stage3.port_names ~f:(fun n s -> s -- ("stage3$" ^ n))
      |> Stage4.create ~scope ~clock ~enable ~p ~config
      |> Stage4.map2 Stage4.port_names ~f:(fun n s -> s -- ("stage4$" ^ n))
    in
    { O.valid; a_mod_p = uresize a_mod_p bits }
  ;;

  let hierarchical ~config ~p scope i =
    let module H = Hierarchy.In_scope (I) (O) in
    let name = [%string "barrett_reduction_%{bits#Int}"] in
    H.hierarchical ~scope ~name (create ~config ~p) i
  ;;
end

let hierarchical ~scope ~config ~p ~clock ~enable { With_valid.valid; value = a } =
  let bits = width a in
  let module M =
    With_interface (struct
      let bits = (bits + 1) / 2
    end)
  in
  let { M.O.a_mod_p; valid } =
    M.hierarchical ~config ~p scope { M.I.clock; enable; a; valid }
  in
  { With_valid.valid; value = a_mod_p }
;;
