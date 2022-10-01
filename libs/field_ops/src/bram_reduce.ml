open Core
open Hardcaml
open Signal

module type Config = sig
  val p : Z.t
  val adder_stages : int
  val num_bits : int
  val error_bits : int
end

module Make (Config : Config) = struct
  open Config

  let sub_pipe ~scope ~stages ~latency ~clock a b =
    assert (width a = width b);
    let enable = vdd in
    let spec = Reg_spec.create ~clock () in
    let res =
      Adder_subtractor_pipe.sub ~scope ~clock ~enable ~stages [ a; b ]
      |> fun { Adder_subtractor_pipe.O.carry; result } ->
      carry @: result
      |> fun x ->
      let n = latency - Adder_subtractor_pipe.latency ~stages in
      assert (n >= 0);
      if n = 0 then x else pipeline spec ~n ~enable x
    in
    [%test_result: int] (width res) ~expect:(width a + 1);
    res
  ;;

  module I = struct
    type 'a t =
      { clock : 'a
      ; coarse_value : 'a [@bits num_bits + error_bits]
      }
    [@@deriving sexp_of, hardcaml ~rtlprefix:"i$"]
  end

  module O = struct
    type 'a t = { reduced_value : 'a [@bits num_bits] } [@@deriving sexp_of, hardcaml]
  end

  (* Perform the ROM lookup for a coarse reduction value *)
  module Stage0 = struct
    let latency = 1

    module I = I

    module O = struct
      type 'a t =
        { coarse_correction : 'a [@bits num_bits]
        ; coarse_value : 'a [@bits num_bits]
        ; top_zero : 'a
        }
      [@@deriving sexp_of, hardcaml ~rtlprefix:"o"]
    end

    let log2_depth = error_bits

    let mux_list =
      assert (0 < log2_depth && log2_depth <= 9);
      (* CR rahuly: very easy to generalize this *)
      assert (Z.(equal p Field_ops_model.Approx_msb_multiplier_model.p));
      (* at most one BRAM deep *)
      (* we can knock off the top [log2_depth] bits here because we know what they are *)
      let tbl =
        Field_ops_model.Approx_msb_multiplier_model.build_precompute_two log2_depth
      in
      List.init (1 lsl log2_depth) ~f:(fun i ->
        let orig_bits =
          Hashtbl.find_exn tbl i |> Bits.of_z ~width:(num_bits + log2_depth)
        in
        let expected_prefix =
          (if i = 0 then 0 else i - 1) |> Bits.of_int ~width:log2_depth
        in
        [%test_result: Bits.t] (Bits.sel_top orig_bits log2_depth) ~expect:expected_prefix;
        let truncated_suffix = Bits.drop_top orig_bits log2_depth in
        [%test_result: int] (Bits.width truncated_suffix) ~expect:num_bits;
        truncated_suffix |> Bits.to_constant |> Signal.of_constant)
    ;;

    let create ~(build_mode : Build_mode.t) scope { I.clock; coarse_value } =
      let ( -- ) = Scope.naming scope in
      let spec = Reg_spec.create ~clock () in
      (* use the top [error_bits] to look up the coarse correction value *)
      let rd_idx =
        let slice = if error_bits = 0 then gnd else sel_top coarse_value error_bits in
        (*print_s [%message (error_bits : int) (log2_depth : int)];*)
        uresize slice log2_depth
      in
      (* perform the ROM read *)
      let coarse_correction =
        match build_mode with
        | Simulation | Synthesis -> mux rd_idx mux_list |> pipeline spec ~n:latency
      in
      ignore (rd_idx -- "rd_idx" : Signal.t);
      ignore (coarse_correction -- "bram_read" : Signal.t);
      (* pipeline the coarse value to stay in sync with the ROM read *)
      let coarse_value = pipeline spec ~n:latency (drop_top coarse_value error_bits) in
      let top_zero = pipeline spec ~n:latency (rd_idx ==:. 0) in
      { O.coarse_correction; coarse_value; top_zero }
    ;;

    let hierarchical ~build_mode ?instance scope =
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical ?instance ~name:"bram_reduce_stage0" ~scope (create ~build_mode)
    ;;
  end

  (* do a coarse reduction from [0,512M] to [0,4M) (conservatvely) - for our particular prime, it's
   * actually just [0, 3M) *)
  module Stage1 = struct
    let latency = adder_stages

    module I = struct
      type 'a t =
        { clock : 'a
        ; stage0 : 'a Stage0.O.t
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t = { coarse_reduction : 'a [@bits num_bits + 1] }
      [@@deriving sexp_of, hardcaml]
    end

    let create scope { I.clock; stage0 = { coarse_value; coarse_correction; top_zero } } =
      let ( -- ) = Scope.naming scope in
      let spec = Reg_spec.create ~clock () in
      let signed_res =
        sub_pipe
          ~scope
          ~stages:adder_stages
          ~latency
          ~clock
          coarse_value
          coarse_correction
      in
      [%test_result: int] (width signed_res) ~expect:(num_bits + 1);
      ignore (signed_res -- "signed_res" : Signal.t);
      (* when [rd_idx > 0], we know that the value we read from the bram has [rd_idx - 1] as its
       * top [num_extra_bits] - after subtracting the [num_bits] suffixes of [v] and [bram], we
       * can just flip the top bit to fix the result 
       * on the other hand, when [rd_idx == 0], we subtract 0 and just keep going to fine 
       * reduction. *)
      let top_zero = pipeline spec ~n:latency top_zero in
      let corrected_msb = mux2 top_zero gnd ~:(msb signed_res) in
      let coarse_reduction = corrected_msb @: lsbs signed_res in
      { O.coarse_reduction }
    ;;

    let hierarchical ?instance scope =
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical ?instance ~name:"bram_reduce_stage1" ~scope create
    ;;
  end

  (* Do a fine reduction from [0, 3M) to [0, M) *)
  module Stage2 = struct
    let latency = adder_stages

    module I = struct
      type 'a t =
        { clock : 'a
        ; stage1 : 'a Stage1.O.t
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = O

    let create scope { I.clock; stage1 = { coarse_reduction } } =
      (*let ( -- ) = Scope.naming scope in*)
      let spec = Reg_spec.create ~clock () in
      let reduced_value =
        List.map [ 2; 1 ] ~f:(fun i ->
          let sub_val = Signal.of_z Z.(of_int i * p) ~width:(num_bits + 1) in
          let res =
            sub_pipe ~scope ~stages:adder_stages ~latency ~clock coarse_reduction sub_val
          in
          [%test_result: int] (width res) ~expect:(num_bits + 2);
          { With_valid.valid = ~:(msb res); value = lsbs res })
        |> priority_select_with_default
             ~default:(pipeline spec coarse_reduction ~n:latency)
        |> Fn.flip uresize num_bits
      in
      { O.reduced_value }
    ;;

    let hierarchical ?instance scope =
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical ?instance ~name:"bram_reduce_stage2" ~scope create
    ;;
  end

  let latency = Stage0.latency + Stage1.latency + Stage2.latency
  let () = assert (latency = 1 + (2 * adder_stages))

  let create ~(build_mode : Build_mode.t) scope (i : _ I.t) =
    let clock = i.clock in
    let stage0 = Stage0.hierarchical ~build_mode scope i in
    let stage1 = Stage1.(hierarchical scope { I.clock; stage0 }) in
    let stage2 = Stage2.(hierarchical scope { I.clock; stage1 }) in
    stage2
  ;;

  let hierarchical ?(build_mode = Build_mode.Simulation) ?instance scope =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ?instance ~name:"bram_reduce" ~scope (create ~build_mode)
  ;;
end
