(** Converts the scalars to a window-compressed form. *)

open Core
open Hardcaml
open Signal
module Axi512 = Hardcaml_axi.Axi512

module Make (Config : Config.S) = struct
  open Config
  open Config_utils.Make (Config)

  (* CR rahuly: taken from [Merge_axi_streams] - share this in a config somewhere *)
  (* Host Streaming Assumptions:
   * - Points: Assume 64b alignment for coordinates, 512b alignment for points 
   * - Scalars: Assume 64b alignment for scalars, packed in whole numbers to 512b words 
   * (i.e. for 253-bit scalars, they are packed 2-per ddr word)
   *)
  let num_64b_words bits = Int.round_up bits ~to_multiple_of:64 / 64
  let num_scalar_64b_words = num_64b_words Config.scalar_bits
  let num_ddr_64b_words = 512 / 64
  let num_scalars_per_ddr_word = num_ddr_64b_words / num_scalar_64b_words
  let log2_num_scalars_per_ddr_word = Int.ceil_log2 num_scalars_per_ddr_word

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; host_scalars_to_fpga : 'a Axi512.Stream.Source.t
           [@rtlprefix "host_scalars_to_fpga$src$"]
      ; transformed_scalars_to_fpga_dest : 'a Axi512.Stream.Dest.t
           [@rtlprefix "transformed_scalars_to_fpga$dst$"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { transformed_scalars_to_fpga : 'a Axi512.Stream.Source.t
           [@rtlprefix "transformed_scalars_to_fpga$src$"]
      ; host_scalars_to_fpga_dest : 'a Axi512.Stream.Dest.t
           [@rtlprefix "host_scalars_to_fpga$dst$"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Scalar_compute_array = struct
    module Scalar_compute = struct
      module T = struct
        type 'a t =
          { (* actually use the correct bit widths, just pad with 0s for interface *)
            windows : 'a array
               [@bits max_window_size_bits] [@length num_windows] [@rtlprefix "windows$"]
          ; carry : 'a
              (* don't bother with tlast because [Merge_axi_streams] doesn't use the scalar tlast anyways. *)
          }
        [@@deriving sexp_of, hardcaml]

        let _sum_of_port_widths = fold port_widths ~init:0 ~f:( + )
      end

      include T
      module With_valid = With_valid.Wrap.Make (T)

      let unpack_from_axi_scalar axi_scalar =
        let windows =
          Array.map2_exn window_bit_sizes window_bit_offsets ~f:(fun size offset ->
            axi_scalar.:+[offset, Some size] |> Fn.flip uresize max_window_size_bits)
        in
        { windows; carry = gnd }
      ;;

      (* apply the top carry and pack - everything is signed now (except the top window) *)
      let pack_to_axi_scalar { windows; carry } =
        let windows =
          Array.mapi
            ~f:(fun i v -> if i = num_windows - 1 then Uop.(v +: carry) else v)
            windows
        in
        Array.map2_exn windows window_bit_sizes ~f:(fun v size -> sresize v size)
        |> Array.to_list
        |> concat_lsb
        |> Fn.flip uresize (64 * num_scalar_64b_words)
      ;;
    end

    module T = struct
      type 'a t =
        { scalars : 'a Scalar_compute.t array [@length num_scalars_per_ddr_word] }
      [@@deriving sexp_of, hardcaml]

      let sum_of_port_widths = fold port_widths ~init:0 ~f:( + )
    end

    include T
    module With_valid = With_valid.Wrap.Make (T)
  end

  module Pipeline_stage = struct
    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; up_scalar : 'a Scalar_compute_array.With_valid.t [@rtlprefix "i_up_scalar$"]
        ; dn_ready : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t =
        { dn_scalar : 'a Scalar_compute_array.With_valid.t [@rtlprefix "o_dn_scalar$"]
        ; up_ready : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end
  end

  module Register_stage = struct
    module I = Pipeline_stage.I
    module O = Pipeline_stage.O

    let data_bits = Int.round_up Scalar_compute_array.sum_of_port_widths ~to_multiple_of:8

    module Axi_wrapper = Hardcaml_axi.Make (struct
      let addr_bits = 32
      let data_bits = data_bits
    end)

    module Register = Axi_wrapper.Stream.Register

    let create scope ({ clock; clear; up_scalar; dn_ready } : _ I.t) : _ O.t =
      let up =
        { (Axi_wrapper.Stream.Source.Of_signal.of_int 0) with
          tdata = uresize (Scalar_compute_array.Of_signal.pack up_scalar.value) data_bits
        ; tvalid = up_scalar.valid
        }
      in
      let dn_dest = { Axi_wrapper.Stream.Dest.tready = dn_ready } in
      let register = Register.hierarchical scope { clock; clear; up; dn_dest } in
      let dn_scalar =
        { With_valid.valid = register.dn.tvalid
        ; value =
            Scalar_compute_array.Of_signal.unpack (sel_bottom register.dn.tdata data_bits)
        }
      in
      let up_ready = register.up_dest.tready in
      { O.dn_scalar; up_ready }
    ;;

    let hierarchical ?instance scope =
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical ?instance ~name:"register_stage" ~scope create
    ;;
  end

  module Minus_one_stage = struct
    module I = Pipeline_stage.I
    module O = Pipeline_stage.O

    let create scope ~stage ({ clock; clear; up_scalar; dn_ready } : _ I.t) : _ O.t =
      assert (0 < stage && stage < num_windows);
      let ( -- ) = Scope.naming scope in
      (* do a bunch of handshaking *)
      let up_ready = wire 1 in
      let reg_with_handshake v =
        let spec = Reg_spec.create ~clock ~clear () in
        reg spec v ~enable:up_ready
      in
      let dn_valid = reg_with_handshake up_scalar.valid in
      (* we can accept whenever we are currently unoccupied, or we are occupied and being drained *)
      up_ready <== (~:dn_valid |: (dn_valid &: dn_ready));
      ignore (up_scalar.valid -- "0_up$0_src$valid" : Signal.t);
      ignore (dn_ready -- "1_dn$1_dst$ready" : Signal.t);
      ignore (dn_valid -- "1_dn$0_src$valid" : Signal.t);
      ignore (up_ready -- "0_up$1_dst$ready" : Signal.t);
      let dn_scalars =
        Array.mapi up_scalar.value.scalars ~f:(fun j up_scalar ->
          Array.iteri up_scalar.windows ~f:(fun i v ->
            ignore
              (v -- Printf.sprintf "0_up$0_src$scalar_%02d$windows$w_%02d" j i : Signal.t));
          ignore
            (up_scalar.carry -- Printf.sprintf "0_up$0_src$scalar_%02d$carry" j
              : Signal.t);
          (* compute window and carry widths *)
          let c_prev = window_bit_sizes.(stage - 1) in
          (* first perform the carry on the previous bucket *)
          let up_prev_bucket_with_overflow =
            let v = sel_bottom up_scalar.windows.(stage - 1) c_prev in
            Uop.(v +: up_scalar.carry)
          in
          assert (width up_prev_bucket_with_overflow = c_prev + 1);
          (* only a 1 bit overflow *)
          let dn_prev_overflow, dn_prev_bucket_unsigned =
            msb up_prev_bucket_with_overflow, lsbs up_prev_bucket_with_overflow
          in
          (* if x_{i-1} >= 2^{c-1}, then use (x_{i-1} - 2^c) instead 
           * assume that carry is small compared to window, so we never have to both overflow and negate
           *)
          let dn_prev_negate = msb dn_prev_bucket_unsigned in
          let dn_prev_signed_and_shifted =
            let v = uresize dn_prev_bucket_unsigned (c_prev + 1) in
            let shift_value = of_int (1 lsl c_prev) ~width:(c_prev + 1) in
            let shifted = sel_bottom Sop.(v -: shift_value) c_prev in
            shifted
          in
          let dn_scalar =
            (* just handshake everything, then overwrite the relevant fields *)
            let registered_up =
              Scalar_compute_array.Scalar_compute.map up_scalar ~f:reg_with_handshake
            in
            let carry = reg_with_handshake (dn_prev_overflow |: dn_prev_negate) in
            let windows = registered_up.windows in
            windows.(stage - 1)
              <- reg_with_handshake
                   (uresize
                      (mux2
                         dn_prev_negate
                         dn_prev_signed_and_shifted
                         dn_prev_bucket_unsigned)
                      max_window_size_bits);
            { Scalar_compute_array.Scalar_compute.windows; carry }
          in
          Array.iteri dn_scalar.windows ~f:(fun i v ->
            ignore
              (v -- Printf.sprintf "1_dn$0_src$scalar_%02d$windows$w_%02d" j i : Signal.t));
          ignore
            (dn_scalar.carry -- Printf.sprintf "1_dn$0_src$scalar_%02d$carry" j
              : Signal.t);
          dn_scalar)
      in
      { O.dn_scalar = { With_valid.valid = dn_valid; value = { scalars = dn_scalars } }
      ; up_ready
      }
    ;;

    let hierarchical ?instance ~stage scope =
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical ?instance ~name:"minus_one_stage" ~scope (create ~stage)
    ;;
  end

  module Stage = struct
    type t =
      | Register
      | Minus_1 of int
  end

  let rec construct_pipeline
    ~outer_dn_scalar
    ~outer_dn_ready
    ~clock
    ~clear
    ~scope
    stages
    up_scalar
    =
    let stages_hd, stages_tl = List.hd_exn stages, List.tl_exn stages in
    let dn_ready = wire 1 in
    let { Pipeline_stage.O.dn_scalar; up_ready; _ } =
      match (stages_hd : Stage.t) with
      | Register ->
        Register_stage.hierarchical
          ~instance:"r_stage"
          scope
          { clock; clear; up_scalar; dn_ready }
      | Minus_1 stage ->
        Minus_one_stage.hierarchical
          ~instance:("1_stage_" ^ Printf.sprintf "%02d" stage)
          scope
          ~stage
          { clock; clear; up_scalar; dn_ready }
    in
    let next_up_ready =
      if List.length stages_tl = 0
      then (
        Scalar_compute_array.With_valid.Of_signal.( <== ) outer_dn_scalar dn_scalar;
        outer_dn_ready)
      else
        (* continue construting the downstream pipeline *)
        construct_pipeline
          ~outer_dn_scalar
          ~outer_dn_ready
          ~scope
          ~clock
          ~clear
          stages_tl
          dn_scalar
    in
    dn_ready <== next_up_ready;
    up_ready
  ;;

  (* A set of modules for streaming to and from an Axi512 interface. This is useful if you want to
   * use a narrow transformation pipeline and only pass through one scalar at a time. We instead
   * scale out multiple transformation pipelines in parallel and transform all the scalars in one
   * word in parallel *)

  module Axi_to_pipeline = struct
    (* Purposefully does no registering; you can add [Register] in the downstream pipeline or skid 
     * buffers upstream if you want *)
    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; host_scalars_to_fpga : 'a Axi512.Stream.Source.t
        ; dn_ready : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t =
        { dn_scalar : 'a Scalar_compute_array.Scalar_compute.With_valid.t
             [@rtlprefix "o_dn_scalar$"]
        ; host_scalars_to_fpga_dest : 'a Axi512.Stream.Dest.t
        }
      [@@deriving sexp_of, hardcaml]
    end

    let create scope (i : _ I.t) : _ O.t =
      let ( -- ) = Scope.naming scope in
      let max = num_scalars_per_ddr_word - 1 in
      let spec = Reg_spec.create () ~clock:i.clock in
      let dn_valid = i.host_scalars_to_fpga.tvalid in
      let scalar_num =
        reg_fb spec ~width:log2_num_scalars_per_ddr_word ~f:(fun v ->
          mux2 (dn_valid &: i.dn_ready) (mod_counter v ~max) v)
      in
      let cur_scalar =
        mux
          scalar_num
          (split_lsb i.host_scalars_to_fpga.tdata ~part_width:(64 * num_scalar_64b_words))
      in
      let dn_scalar =
        { With_valid.valid = dn_valid
        ; value = Scalar_compute_array.Scalar_compute.unpack_from_axi_scalar cur_scalar
        }
      in
      let tready = i.dn_ready &: (scalar_num ==:. max) in
      (* label signals *)
      (* dn_scalar *)
      Array.iteri dn_scalar.value.windows ~f:(fun i v ->
        ignore (v -- Printf.sprintf "1_dn$0_src$windows$w_%02d" i : Signal.t));
      ignore (dn_valid -- "1_dn$0_src$valid" : Signal.t);
      ignore (i.dn_ready -- "1_dn$1_dst$ready" : Signal.t);
      { dn_scalar; host_scalars_to_fpga_dest = { tready } }
    ;;

    let _hierarchical ?instance scope =
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical ?instance ~name:"axi_to_pipeline" ~scope create
    ;;
  end

  module Pipeline_to_axi = struct
    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; up_scalar : 'a Scalar_compute_array.Scalar_compute.With_valid.t
             [@rtlprefix "i_up_scalar$"]
        ; transformed_scalars_to_fpga_dest : 'a Axi512.Stream.Dest.t
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t =
        { transformed_scalars_to_fpga : 'a Axi512.Stream.Source.t
        ; up_ready : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    let create scope (i : _ I.t) : _ O.t =
      let ( -- ) = Scope.naming scope in
      let spec = Reg_spec.create () ~clock:i.clock in
      let max = num_scalars_per_ddr_word - 1 in
      let dn_ready = i.transformed_scalars_to_fpga_dest.tready in
      let up_ready = wire 1 in
      (* label signals *)
      (* up_scalar *)
      Array.iteri i.up_scalar.value.windows ~f:(fun i v ->
        ignore (v -- Printf.sprintf "1_up$0_src$windows$w_%02d" i : Signal.t));
      ignore (i.up_scalar.valid -- "1_up$0_src$valid" : Signal.t);
      ignore (up_ready -- "1_up$1_dst$ready" : Signal.t);
      let shift_in = i.up_scalar.valid &: up_ready in
      ignore (shift_in -- "shift_in" : Signal.t);
      (* shift scalars in - transform and shift 
       *  - shift in from the left to match the scalar stream ordering - lsb comes first
       *)
      let next_scalar_in =
        Scalar_compute_array.Scalar_compute.pack_to_axi_scalar i.up_scalar.value
      in
      ignore (next_scalar_in -- "next_scalar_in" : Signal.t);
      let shift_in_data =
        reg_fb
          spec
          ~width:(64 * num_scalar_64b_words * max)
          ~f:(fun v ->
            let shifted_v =
              if width v = 64 * num_scalar_64b_words
              then next_scalar_in
              else drop_top v (64 * num_scalar_64b_words) @: next_scalar_in
            in
            mux2 shift_in shifted_v v)
      in
      let full_shift = shift_in_data @: next_scalar_in in
      ignore (full_shift -- "full_shift" : Signal.t);
      (* keep track of how many scalars we've shifted in *)
      let scalar_num =
        reg_fb spec ~width:log2_num_scalars_per_ddr_word ~f:(fun v ->
          mux2 shift_in (mod_counter v ~max) v)
      in
      (* construct the downstream *)
      let tvalid = scalar_num ==:. max in
      let transformed_scalars_to_fpga =
        { (Axi512.Stream.Source.Of_signal.of_int 0) with
          tdata = uresize full_shift 512
        ; tvalid
        }
      in
      (* construct the upstream *)
      up_ready <== (scalar_num <:. max |: (scalar_num ==:. max &: dn_ready));
      { O.transformed_scalars_to_fpga; up_ready }
    ;;

    let _hierarchical ?instance scope =
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical ?instance ~name:"pipeline_to_axi" ~scope create
    ;;
  end

  (* extract bits from scalar off the wire *)
  let unpack_to_windows_and_negatives
    (type a)
    (module Comb : Hardcaml.Comb.S with type t = a)
    (axi_scalar : a)
    =
    let open Comb in
    (* given an axi scalar, pull out the negatives *)
    Array.mapi
      (Array.zip_exn window_bit_sizes window_bit_offsets)
      ~f:(fun i (size, offset) ->
      let signed_val = axi_scalar.:+[offset, Some size] in
      let unsigned_val, negative =
        if i = num_windows - 1
        then signed_val, gnd
        else (
          (* CR rayesantharao: can do a bit trick here: (n + mask ) ^ mask (mask = (sra n (w - 1))) *)
          let negative = msb signed_val in
          let bucket = mux2 negative (negate signed_val) signed_val in
          bucket, negative)
      in
      uresize unsigned_val max_window_size_bits, negative)
    |> Array.unzip
  ;;

  let create scope (i : _ I.t) : _ O.t =
    (*let ( -- ) = Scope.naming scope in*)
    (* construct input *)
    (*let pipeline_up_ready = wire 1 in
    let input_from_axi =
      Axi_to_pipeline.hierarchical
        scope
        { clock = i.clock
        ; clear = i.clear
        ; host_scalars_to_fpga = i.host_scalars_to_fpga
        ; dn_ready = pipeline_up_ready
        }
    in*)
    let input_from_axi =
      { With_valid.valid = i.host_scalars_to_fpga.tvalid
      ; value =
          { Scalar_compute_array.scalars =
              Array.init num_scalars_per_ddr_word ~f:(fun scalar_num ->
                let size = 64 * num_scalar_64b_words in
                let offset = scalar_num * size in
                let axi_scalar =
                  i.host_scalars_to_fpga.tdata.:+[offset, Some Config.scalar_bits]
                in
                Scalar_compute_array.Scalar_compute.unpack_from_axi_scalar axi_scalar)
          }
      }
    in
    (* construct pipeline *)
    let pipeline_dn_scalar = Scalar_compute_array.With_valid.Of_signal.wires () in
    let pipeline_dn_ready = i.transformed_scalars_to_fpga_dest.tready in
    let pipeline_up_ready =
      construct_pipeline
        ~outer_dn_scalar:pipeline_dn_scalar
        ~outer_dn_ready:pipeline_dn_ready
        ~scope
        ~clock:i.clock
        ~clear:i.clear
        (List.init (num_windows + 1) ~f:(fun i ->
           if i = 0
           then Stage.Register
           else if 1 <= i && i <= num_windows - 1
           then Minus_1 i
           else if i = num_windows
           then Register
           else failwith "bad index"))
        input_from_axi
    in
    (*let output_to_axi =
      Pipeline_to_axi.hierarchical
        scope
        { clock = i.clock
        ; clear = i.clear
        ; up_scalar = pipeline_dn_scalar
        ; transformed_scalars_to_fpga_dest = 
        }
    in*)
    let output_to_axi =
      { (Axi512.Stream.Source.Of_signal.of_int 0) with
        tvalid = pipeline_dn_scalar.valid
      ; tdata =
          Array.map
            pipeline_dn_scalar.value.scalars
            ~f:Scalar_compute_array.Scalar_compute.pack_to_axi_scalar
          |> Array.to_list
          |> concat_lsb
          |> Fn.flip uresize 512
      }
    in
    { O.host_scalars_to_fpga_dest = { tready = pipeline_up_ready }
    ; transformed_scalars_to_fpga = output_to_axi
    }
  ;;

  let hierarchical ?instance scope =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ?instance ~name:"scalar_transformation" ~scope create
  ;;
end
