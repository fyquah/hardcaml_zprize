open Core
open Hardcaml
open Signal

include struct
  open Field_ops_lib
  module Arbitrate = Arbitrate
  module Adder_subtractor_pipe = Adder_subtractor_pipe
  module Modulo_adder_pipe = Modulo_adder_pipe
  module Modulo_subtractor_pipe = Modulo_subtractor_pipe
  module Bram_reduce = Bram_reduce
end

include struct
  open Elliptic_curve_lib
  module Config_presets = Config_presets
  module Ec_fpn_ops_config = Ec_fpn_ops_config
end

module Model = Twisted_edwards_model_lib
module Modulo_ops = Model.Bls12_377_util.Modulo_ops

module Make (Num_bits : Num_bits.S) = struct
  open Num_bits
  module Xyt = Xyt.Make (Num_bits)
  module Xyzt = Xyzt.Make (Num_bits)

  module I = struct
    type 'a t =
      { clock : 'a
      ; valid_in : 'a
      ; p1 : 'a Xyzt.t [@rtlprefix "p1$"]
      ; p2 : 'a Xyt.t [@rtlprefix "p2$"]
      ; subtract : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { valid_out : 'a
      ; p3 : 'a Xyzt.t [@rtlprefix "p3$"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let multiply ~(config : Config.t) ~scope ~clock ~latency ~include_fine_reduction (x, y) =
    let enable = vdd in
    assert (width x = width y);
    let scope = Scope.sub_scope scope "multiply" in
    let reduce = if include_fine_reduction then Config.reduce else Config.coarse_reduce in
    let final_pipelining =
      latency config
      - Config.multiply_latency
          ~reduce:(if include_fine_reduction then Fine else Coarse)
          config
    in
    assert (final_pipelining >= 0);
    config.multiply.impl ~scope ~clock ~enable x (Some y)
    |> reduce config ~scope ~clock ~enable
    |> pipeline (Reg_spec.create ~clock ()) ~enable ~n:final_pipelining
  ;;

  let double_multiply
    ~include_fine_reduction
    ~(config : Config.t)
    ~scope
    ~clock
    ~valid
    ~latency
    ~latency_without_arbitration
    (x1, y1)
    (x2, y2)
    =
    let enable = vdd in
    assert (width y1 = width y2);
    assert (width x1 = width x2);
    let wy = width y1 in
    let wx = width x1 in
    let multiply ?(scope = scope) ~latency (a, b) =
      multiply ~scope ~config ~clock ~latency ~include_fine_reduction (a, b)
    in
    if config.arbitrated_multiplier
    then (
      let scope = Scope.sub_scope scope "arbed_multiply" in
      Arbitrate.arbitrate2
        (x1 @: y1, x2 @: y2)
        ~enable
        ~clock
        ~valid
        ~f:(fun input ->
          let y = sel_bottom input wy in
          let x = sel_top input wx in
          multiply (x, y) ~scope ~latency:latency_without_arbitration))
    else multiply ~latency (x1, y1), multiply ~latency (x2, y2)
  ;;

  let concat_result { Adder_subtractor_pipe.O.carry; result } = carry @: result

  let add_pipe ~scope ~latency ~(config : Config.t) ~clock a b =
    assert (width a = width b);
    let spec = Reg_spec.create ~clock () in
    let stages = config.adder_stages in
    let enable = vdd in
    let res =
      Adder_subtractor_pipe.add ~scope ~clock ~enable ~stages [ a; b ]
      |> concat_result
      |> fun x ->
      let n = latency config - Adder_subtractor_pipe.latency ~stages in
      assert (n >= 0);
      if n = 0 then x else pipeline spec ~n ~enable x
    in
    [%test_result: int] (width res) ~expect:(width a + 1);
    (*print_s [%message "add_pipe" (width res : int) (width a : int)];*)
    res
  ;;

  let sub_pipe ~scope ~latency ~(config : Config.t) ~clock a b =
    assert (width a = width b);
    let enable = vdd in
    let width = width a in
    let spec = Reg_spec.create ~clock () in
    let stages = config.subtractor_stages in
    let extra_bits = width - num_bits in
    Adder_subtractor_pipe.mixed
      ~scope
      ~clock
      ~enable
      ~stages
      ~init:a
      [ Sub b; Add (Signal.of_z Z.(config.p lsl extra_bits) ~width) ]
    |> concat_result
    |> fun x ->
    let n = latency config - Adder_subtractor_pipe.latency ~stages in
    assert (n >= 0);
    if n = 0 then x else pipeline spec ~n ~enable x
  ;;

  module Reduce_stage (X : sig
    module O : Interface.S

    val error : int
    val accumulated_error : int
  end) (Y : sig
    val port_descrs : [ `Value | `Valid ] X.O.t
    val name : string
    val slr : Config.t -> int option
  end) =
  struct
    let accumulated_error = X.(error + accumulated_error)

    module Bram_reduce_of_config (C : sig
      val config : Config.t
    end) =
    struct
      open C

      module Bram_config = struct
        let p = config.p
        let adder_stages = config.adder_stages
        let num_bits = num_bits
        let error_bits = accumulated_error
      end

      module Bram_reduce = Bram_reduce.Make (Bram_config)
    end

    module I = struct
      type 'a t =
        { clock : 'a
        ; i_data : 'a X.O.t [@rtlprefix "i_data_"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      module O_data =
        Interface.Update
          (X.O)
          (struct
            let t =
              X.O.map2 X.O.port_names Y.port_descrs ~f:(fun a b ->
                ( "o_data_" ^ a
                , match b with
                  | `Valid -> 1
                  | `Value -> num_bits ))
            ;;
          end)

      type 'a t = { o_data : 'a O_data.t } [@@deriving sexp_of, hardcaml]
    end

    let latency (config : Config.t) =
      let module B =
        Bram_reduce_of_config (struct
          let config = config
        end)
      in
      B.Bram_reduce.latency
    ;;

    let create ~build_mode ~(config : Config.t) scope { I.clock; i_data } =
      let scope = Scope.sub_scope scope "reduce_stage" in
      let module B =
        Bram_reduce_of_config (struct
          let config = config
        end)
      in
      (* wrap the bram reduction *)
      let reduce coarse_value =
        let reduce =
          B.Bram_reduce.(hierarchical ~build_mode scope { I.clock; coarse_value })
        in
        reduce.reduced_value
      in
      let pipe =
        let spec = Reg_spec.create ~clock () in
        pipeline spec ~n:(latency config)
      in
      { O.o_data =
          X.O.map3 i_data X.O.port_names Y.port_descrs ~f:(fun v name descr ->
            (match descr with
             | `Valid -> pipe v
             | `Value ->
               [%test_result: int] (width v) ~expect:B.Bram_config.(num_bits + error_bits);
               reduce v)
            |> fun x -> Scope.naming scope x name)
      }
    ;;

    let hierarchical ~build_mode ~(config : Config.t) scope i =
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical
        ~scope
        ~name:(sprintf "mixed_add_precompute_%s" Y.name)
        ?instance:
          (match Y.slr config with
           | None -> None
           | Some slr -> Some (sprintf "mixed_add_precompute_%s_SLR%d" Y.name slr))
        (create ~build_mode ~config)
        i
    ;;
  end

  module Datapath_input = struct
    type 'a t =
      { p1 : 'a Xyzt.t [@rtlprefix "p1$"]
      ; p2 : 'a Xyt.t [@rtlprefix "p2$"]
      ; subtract : 'a [@rtlname "input_subtract"]
      ; valid : 'a [@rtlname "input_valid"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Stage0a = struct
    let include_fine_reduction = false

    (* CR rahul: need to get this correctly from the config - it's the log2 of error introduced by 
     * the msb approximation *)
    let error = if include_fine_reduction then 0 else 4

    module I = struct
      type 'a t =
        { clock : 'a
        ; datapath_input : 'a Datapath_input.t
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t =
        { c_A : 'a [@bits num_bits + error]
        ; c_B : 'a [@bits num_bits + error]
        ; subtract : 'a [@rtlname "stage0_subtract"]
        ; valid : 'a [@rtlname "stage0_valid"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    let latency_without_arbitration (config : Config.t) =
      Config.multiply_latency
        ~reduce:(if include_fine_reduction then Fine else Coarse)
        config
    ;;

    let latency (config : Config.t) =
      latency_without_arbitration config + if config.arbitrated_multiplier then 1 else 0
    ;;

    let create
      ~config
      scope
      { I.clock; datapath_input = { Datapath_input.p1; p2; subtract; valid } }
      =
      [%test_result: int] (width p1.x) ~expect:num_bits;
      [%test_result: int] (width p1.y) ~expect:num_bits;
      [%test_result: int] (width p2.x) ~expect:num_bits;
      [%test_result: int] (width p2.y) ~expect:num_bits;
      let spec = Reg_spec.create ~clock () in
      let pipe = pipeline spec ~n:(latency config) in
      let c_A, c_B =
        double_multiply
          ~include_fine_reduction
          ~config
          ~scope
          ~clock
          ~valid
          ~latency
          ~latency_without_arbitration
          (p1.x, p2.x)
          (p1.y, p2.y)
      in
      let scope = Scope.sub_scope scope "stage0a" in
      { c_A; c_B; subtract = pipe subtract; valid = pipe valid }
      |> O.map2 O.port_names ~f:(fun name x -> Scope.naming scope x name)
    ;;

    let hierarchical ~(config : Config.t) scope i =
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical
        ~scope
        ~name:"mixed_add_precompute_stage_0a"
        ?instance:
          (match config.slr_assignments.stage0a with
           | None -> None
           | Some slr -> Some (sprintf "mixed_add_precompute_stage_0a_SLR%d" slr))
        (create ~config)
        i
    ;;
  end

  module Stage0b = struct
    let include_fine_reduction = false

    (* CR rahul: need to get this correctly from the config - it's the log2 of error introduced by 
     * the msb approximation *)
    let error = if include_fine_reduction then 0 else 4

    module I = struct
      type 'a t =
        { clock : 'a
        ; datapath_input : 'a Datapath_input.t
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t =
        { c_C : 'a [@bits num_bits + error]
        ; c_D : 'a [@bits num_bits]
        ; subtract : 'a [@rtlname "stage0b_subtract"]
        ; valid : 'a [@rtlname "stage0b_valid"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    let latency_without_arbitration (config : Config.t) =
      Config.multiply_latency
        ~reduce:(if include_fine_reduction then Fine else Coarse)
        config
    ;;

    let latency (config : Config.t) =
      latency_without_arbitration config + if config.arbitrated_multiplier then 1 else 0
    ;;

    let create
      ~config
      scope
      { I.clock; datapath_input = { Datapath_input.p1; p2; subtract; valid } }
      =
      [%test_result: int] (width p1.x) ~expect:num_bits;
      [%test_result: int] (width p1.y) ~expect:num_bits;
      [%test_result: int] (width p2.x) ~expect:num_bits;
      [%test_result: int] (width p2.y) ~expect:num_bits;
      let spec = Reg_spec.create ~clock () in
      let pipe = pipeline spec ~n:(latency config) in
      let c_C =
        multiply ~include_fine_reduction ~latency ~config ~scope ~clock (p1.t, p2.t)
      in
      let scope = Scope.sub_scope scope "stage0b" in
      { c_C; c_D = pipe p1.z; subtract = pipe subtract; valid = pipe valid }
      |> O.map2 O.port_names ~f:(fun name x -> Scope.naming scope x name)
    ;;

    let hierarchical ~(config : Config.t) scope i =
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical
        ~scope
        ~name:"mixed_add_precompute_stage_0b"
        ?instance:
          (match config.slr_assignments.stage0b with
           | None -> None
           | Some slr -> Some (sprintf "mixed_add_precompute_stage_0b_SLR%d" slr))
        (create ~config)
        i
    ;;
  end

  module Stage1 = struct
    let () = assert (Stage0a.error = Stage0b.error)
    let accumulated_error = Stage0a.error
    let error = 1

    module I = struct
      type 'a t =
        { clock : 'a
        ; stage0a : 'a Stage0a.O.t [@rtlprefix "stage0a$"]
        ; stage0b : 'a Stage0b.O.t [@rtlprefix "stage0b$"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t =
        { c_E : 'a [@bits num_bits + accumulated_error + error]
        ; c_F : 'a [@bits num_bits + accumulated_error + error]
        ; c_G : 'a [@bits num_bits + accumulated_error + error]
        ; c_H : 'a [@bits num_bits + accumulated_error + error]
        ; valid : 'a [@rtlname "stage1_valid"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    let latency (config : Config.t) = config.adder_stages

    let create
      ~config
      scope
      { I.clock
      ; stage0a = { c_A; c_B; subtract; valid }
      ; stage0b = { c_C; c_D; subtract = _; valid = _ }
      }
      =
      [%test_result: int] (width c_A) ~expect:(num_bits + accumulated_error);
      [%test_result: int] (width c_B) ~expect:(num_bits + accumulated_error);
      [%test_result: int] (width c_C) ~expect:(num_bits + accumulated_error);
      [%test_result: int] (width c_D) ~expect:num_bits;
      let c_D = uresize c_D (num_bits + accumulated_error) in
      let spec = Reg_spec.create ~clock () in
      let pipe = pipeline spec ~n:(latency config) in
      (* Consider arb-ing here? *)
      let re s = uresize s (num_bits + accumulated_error + error) in
      let c_E = sub_pipe ~scope ~latency ~config ~clock c_B c_A |> re in
      let c_D_minus_c_C = sub_pipe ~scope ~latency ~config ~clock c_D c_C |> re in
      let c_D_plus_c_C = add_pipe ~scope ~latency ~config ~clock c_D c_C |> re in
      let c_H = add_pipe ~scope ~latency ~config ~clock c_B c_A |> re in
      (* assign based on the sign of t2 *)
      let subtract = pipe subtract in
      let c_F = mux2 subtract c_D_plus_c_C c_D_minus_c_C in
      let c_G = mux2 subtract c_D_minus_c_C c_D_plus_c_C in
      let scope = Scope.sub_scope scope "stage2" in
      { c_E; c_F; c_G; c_H; valid = pipe valid }
      |> O.map2 O.port_names ~f:(fun name x -> Scope.naming scope x name)
    ;;

    let hierarchical ~(config : Config.t) scope i =
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical
        ~scope
        ~name:"mixed_add_precompute_stage_1"
        ?instance:
          (match config.slr_assignments.stage2 with
           | None -> None
           | Some slr -> Some (sprintf "mixed_add_precompute_stage_1_SLR%d" slr))
        (create ~config)
        i
    ;;
  end

  module Stage2 =
    Reduce_stage
      (Stage1)
      (struct
        let name = "stage_2"
        let slr (config : Config.t) = config.slr_assignments.stage2

        let port_descrs =
          { Stage1.O.c_E = `Value
          ; c_F = `Value
          ; c_G = `Value
          ; c_H = `Value
          ; valid = `Valid
          }
        ;;
      end)

  module Stage3 = struct
    let include_fine_reduction = false

    (* CR rahul: need to get this correctly from the config - it's the log2 of error introduced by 
     * the msb approximation *)
    let error = if include_fine_reduction then 0 else 4

    module I = struct
      type 'a t =
        { clock : 'a
        ; stage2 : 'a Stage2.O.t
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t =
        { pre_x3 : 'a [@bits num_bits + error]
        ; pre_y3 : 'a [@bits num_bits + error]
        ; z3 : 'a [@bits num_bits + error]
        ; t3 : 'a [@bits num_bits + error]
        ; valid : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    let latency_without_arbitration (config : Config.t) =
      Config.multiply_latency
        ~reduce:(if include_fine_reduction then Fine else Coarse)
        config
    ;;

    let latency (config : Config.t) =
      latency_without_arbitration config + if config.arbitrated_multiplier then 1 else 0
    ;;

    let create
      ~config
      scope
      { I.clock; stage2 = { o_data = { c_E; c_F; c_G; c_H; valid } } }
      =
      let spec_with_clear = Reg_spec.create ~clock () in
      let pipe_with_clear = pipeline spec_with_clear ~n:(latency config) in
      let pre_x3, pre_y3 =
        double_multiply
          ~config
          ~scope
          ~clock
          ~valid
          ~latency
          ~latency_without_arbitration
          ~include_fine_reduction
          (c_E, c_F)
          (c_G, c_H)
      in
      let t3, z3 =
        double_multiply
          ~config
          ~scope
          ~clock
          ~valid
          ~latency
          ~latency_without_arbitration
          ~include_fine_reduction
          (c_E, c_H)
          (c_F, c_G)
      in
      let scope = Scope.sub_scope scope "stage3" in
      { O.pre_x3; pre_y3; z3; t3; valid = pipe_with_clear valid }
      |> O.map2 O.port_names ~f:(fun name x -> Scope.naming scope x name)
    ;;

    let hierarchical ~(config : Config.t) scope i =
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical
        ~scope
        ~name:"mixed_add_precompute_stage_3"
        ?instance:
          (match config.slr_assignments.stage3 with
           | None -> None
           | Some slr -> Some (sprintf "mixed_add_precompute_stage_3_SLR%d" slr))
        (create ~config)
        i
    ;;
  end

  module Stage4 = struct
    let accumulated_error = Stage3.error
    let error = 1

    module I = struct
      type 'a t =
        { clock : 'a
        ; stage3 : 'a Stage3.O.t [@rtlprefix "stage3_"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t =
        { x3 : 'a [@bits num_bits + accumulated_error + error]
        ; y3 : 'a [@bits num_bits + accumulated_error + error]
        ; z3 : 'a [@bits num_bits + accumulated_error + error]
        ; t3 : 'a [@bits num_bits + accumulated_error + error]
        ; valid : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    let latency (config : Config.t) = config.adder_stages

    let create
      ~config
      scope
      { I.clock; stage3 = { Stage3.O.pre_x3; pre_y3; z3; t3; valid } }
      =
      [%test_result: int] (width pre_x3) ~expect:(num_bits + accumulated_error);
      [%test_result: int] (width pre_y3) ~expect:(num_bits + accumulated_error);
      [%test_result: int] (width z3) ~expect:(num_bits + accumulated_error);
      [%test_result: int] (width t3) ~expect:(num_bits + accumulated_error);
      let spec = Reg_spec.create ~clock () in
      let pipe = pipeline spec ~n:(latency config) in
      assert (config.adder_stages = config.subtractor_stages);
      let re s = uresize s (num_bits + accumulated_error + error) in
      let x3 = sub_pipe ~scope ~latency ~config ~clock pre_y3 pre_x3 |> re in
      let y3 = add_pipe ~scope ~latency ~config ~clock pre_y3 pre_x3 |> re in
      let z3 = pipe z3 |> re in
      let t3 = pipe t3 |> re in
      let scope = Scope.sub_scope scope "stage4" in
      { x3; y3; z3; t3; valid = pipe valid }
      |> O.map2 O.port_names ~f:(fun name x -> Scope.naming scope x name)
    ;;

    let hierarchical ~(config : Config.t) scope i =
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical
        ~scope
        ~name:"mixed_add_precompute_stage_4"
        ?instance:
          (match config.slr_assignments.stage3 with
           | None -> None
           | Some slr -> Some (sprintf "mixed_add_precompute_stage_4_SLR%d" slr))
        (create ~config)
        i
    ;;
  end

  module Stage5 =
    Reduce_stage
      (Stage4)
      (struct
        let name = "stage_5"
        let slr (config : Config.t) = config.slr_assignments.stage5

        let port_descrs =
          { Stage4.O.x3 = `Value; y3 = `Value; z3 = `Value; t3 = `Value; valid = `Valid }
        ;;
      end)

  let output_pipes = 2

  let needs_slr_crossing ~src ~dst =
    match src, dst with
    | None, _ | _, None -> false
    | Some src, Some dst -> src <> dst
  ;;

  let latency (config : Config.t) =
    let slr_assignments = config.slr_assignments in
    let needs_slr_crossing_input_to_stage0a =
      needs_slr_crossing ~src:slr_assignments.input ~dst:slr_assignments.stage0a
    in
    let needs_slr_crossing_input_to_stage0b =
      needs_slr_crossing ~src:slr_assignments.input ~dst:slr_assignments.stage0b
    in
    let needs_slr_crossing_stage0a_to_1 =
      needs_slr_crossing ~src:slr_assignments.stage0a ~dst:slr_assignments.stage1
    in
    let needs_slr_crossing_stage0b_to_1 =
      needs_slr_crossing ~src:slr_assignments.stage0b ~dst:slr_assignments.stage1
    in
    let needs_slr_crossing_stage1_to_2 =
      needs_slr_crossing ~src:slr_assignments.stage1 ~dst:slr_assignments.stage2
    in
    let needs_slr_crossing_stage2_to_3 =
      needs_slr_crossing ~src:slr_assignments.stage2 ~dst:slr_assignments.stage3
    in
    let needs_slr_crossing_stage3_to_4 =
      needs_slr_crossing ~src:slr_assignments.stage3 ~dst:slr_assignments.stage4
    in
    let needs_slr_crossing_stage4_to_5 =
      needs_slr_crossing ~src:slr_assignments.stage4 ~dst:slr_assignments.stage5
    in
    assert (Stage0a.latency config = Stage0b.latency config);
    Stage0a.latency config
    + Int.max
        ((if needs_slr_crossing_input_to_stage0a then 2 else 0)
        + if needs_slr_crossing_stage0a_to_1 then 2 else 0)
        ((if needs_slr_crossing_input_to_stage0b then 2 else 0)
        + if needs_slr_crossing_stage0b_to_1 then 2 else 0)
    + Stage1.latency config
    + (if needs_slr_crossing_stage1_to_2 then 2 else 0)
    + Stage2.latency config
    + (if needs_slr_crossing_stage2_to_3 then 2 else 0)
    + Stage3.latency config
    + (if needs_slr_crossing_stage3_to_4 then 2 else 0)
    + Stage4.latency config
    + (if needs_slr_crossing_stage4_to_5 then 2 else 0)
    + Stage5.latency config
    + output_pipes
  ;;

  let named_register = Field_ops_lib.Named_register.named_register

  let create
    ?(build_mode = Build_mode.Synthesis)
    ~(config : Config.t)
    scope
    { I.clock; valid_in; subtract; p1; p2 }
    =
    let slr_assignments = config.slr_assignments in
    let { Stage5.O.o_data = { x3; y3; z3; t3; valid = valid_out } } =
      let needs_slr_crossing_input_to_stage0a =
        needs_slr_crossing ~src:slr_assignments.input ~dst:slr_assignments.stage0a
      in
      let needs_slr_crossing_input_to_stage0b =
        needs_slr_crossing ~src:slr_assignments.input ~dst:slr_assignments.stage0b
      in
      let needs_slr_crossing_stage0a_to_1 =
        needs_slr_crossing ~src:slr_assignments.stage0a ~dst:slr_assignments.stage1
      in
      let needs_slr_crossing_stage0b_to_1 =
        needs_slr_crossing ~src:slr_assignments.stage0b ~dst:slr_assignments.stage1
      in
      let needs_slr_crossing_stage1_to_2 =
        needs_slr_crossing ~src:slr_assignments.stage1 ~dst:slr_assignments.stage2
      in
      let needs_slr_crossing_stage2_to_3 =
        needs_slr_crossing ~src:slr_assignments.stage2 ~dst:slr_assignments.stage3
      in
      let needs_slr_crossing_stage3_to_4 =
        needs_slr_crossing ~src:slr_assignments.stage3 ~dst:slr_assignments.stage4
      in
      let needs_slr_crossing_stage4_to_5 =
        needs_slr_crossing ~src:slr_assignments.stage4 ~dst:slr_assignments.stage5
      in
      let named_register slr x = named_register ~slr ~clock ~scope x in
      let datapath_input =
        (* when we subtract, we want to output [p1-p2] instead of [p1+p2]. Because p2 is coming
         * from the host, it is represented as ((y-x)/2, (y+x)/2, 4d * t) -> negating a point
         * in twisted edwards is equivalent to negating the x coordinate, so we have to swap x,y and
         * negate t. We push the t negation down the computation to avoid adding another modulo 
         * subtractor. *)
        let p2 = Xyt.Of_signal.mux2 subtract { Xyt.x = p2.y; y = p2.x; t = p2.t } p2 in
        { Datapath_input.p1; p2; subtract; valid = valid_in }
      in
      let stage0a =
        let datapath_input =
          if needs_slr_crossing_input_to_stage0a
          then
            datapath_input
            |> Datapath_input.Of_signal.pack
            |> named_register slr_assignments.input
            |> named_register slr_assignments.stage0a
            |> Datapath_input.Of_signal.unpack
          else datapath_input
        in
        Stage0a.hierarchical ~config scope { clock; datapath_input }
      in
      let stage0b =
        let datapath_input =
          if needs_slr_crossing_input_to_stage0b
          then
            datapath_input
            |> Datapath_input.Of_signal.pack
            |> named_register slr_assignments.input
            |> named_register slr_assignments.stage0b
            |> Datapath_input.Of_signal.unpack
          else datapath_input
        in
        Stage0b.hierarchical ~config scope { clock; datapath_input }
      in
      let stage1 =
        let stage0a =
          if needs_slr_crossing_stage0a_to_1
          then
            stage0a
            |> Stage0a.O.Of_signal.pack
            |> named_register slr_assignments.stage0a
            |> named_register slr_assignments.stage1
            |> Stage0a.O.Of_signal.unpack
          else stage0a
        in
        let stage0b =
          if needs_slr_crossing_stage0b_to_1
          then
            stage0b
            |> Stage0b.O.Of_signal.pack
            |> named_register slr_assignments.stage0b
            |> named_register slr_assignments.stage1
            |> Stage0b.O.Of_signal.unpack
          else stage0b
        in
        let lat_a =
          (if needs_slr_crossing_input_to_stage0a then 2 else 0)
          + if needs_slr_crossing_stage0a_to_1 then 2 else 0
        in
        let lat_b =
          (if needs_slr_crossing_input_to_stage0b then 2 else 0)
          + if needs_slr_crossing_stage0b_to_1 then 2 else 0
        in
        let lat = Int.max lat_a lat_b in
        let stage0a =
          Stage0a.O.map
            ~f:(Signal.pipeline (Reg_spec.create ~clock ()) ~n:(lat - lat_a))
            stage0a
        in
        let stage0b =
          Stage0b.O.map
            ~f:(Signal.pipeline (Reg_spec.create ~clock ()) ~n:(lat - lat_b))
            stage0b
        in
        Stage1.hierarchical ~config scope { clock; stage0a; stage0b }
      in
      let stage2 =
        let stage1 =
          if needs_slr_crossing_stage1_to_2
          then
            stage1
            |> Stage1.O.Of_signal.pack
            |> named_register slr_assignments.stage1
            |> named_register slr_assignments.stage2
            |> Stage1.O.Of_signal.unpack
          else stage1
        in
        Stage2.hierarchical ~build_mode ~config scope { clock; i_data = stage1 }
      in
      let stage3 =
        let stage2 =
          if needs_slr_crossing_stage2_to_3
          then
            stage2
            |> Stage2.O.Of_signal.pack
            |> named_register slr_assignments.stage2
            |> named_register slr_assignments.stage3
            |> Stage2.O.Of_signal.unpack
          else stage2
        in
        Stage3.hierarchical ~config scope { clock; stage2 }
      in
      let stage4 =
        let stage3 =
          if needs_slr_crossing_stage3_to_4
          then
            stage3
            |> Stage3.O.Of_signal.pack
            |> named_register slr_assignments.stage3
            |> named_register slr_assignments.stage4
            |> Stage3.O.Of_signal.unpack
          else stage3
        in
        Stage4.hierarchical ~config scope { clock; stage3 }
      in
      let stage5 =
        let stage4 =
          if needs_slr_crossing_stage4_to_5
          then
            stage4
            |> Stage4.O.Of_signal.pack
            |> named_register slr_assignments.stage4
            |> named_register slr_assignments.stage5
            |> Stage4.O.Of_signal.unpack
          else stage4
        in
        Stage5.hierarchical ~build_mode ~config scope { clock; i_data = stage4 }
      in
      Stage5.O.Of_signal.pack stage5
      |> named_register slr_assignments.stage3
      |> named_register slr_assignments.output
      |> Stage5.O.Of_signal.unpack
    in
    { O.valid_out; p3 = { x = x3; y = y3; z = z3; t = t3 } }
  ;;

  let hierarchical ?build_mode ?instance ~config scope =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ?instance ~name:"adder_precompute" ~scope (create ?build_mode ~config)
  ;;
end
