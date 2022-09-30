(** Converts the scalars to a window-compressed form. *)

open Core
open Hardcaml
open Signal

module Make (Config : Config.S) (Num_bits : Twisted_edwards_lib.Num_bits.S) = struct
  open Config
  open Config_utils.Make (Config)
  module Xyt = Twisted_edwards_lib.Xyt.Make (Num_bits)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; scalar : 'a [@bits scalar_bits]
      ; scalar_valid : 'a
      ; last_scalar : 'a
      ; input_point : 'a Xyt.t [@rtlprefix "i$xyt$"]
      ; scalar_and_input_point_ready : 'a
      }
    [@@deriving sexp_of, hardcaml ~rtlprefix:"i$"]
  end

  module O = struct
    type 'a t =
      { scalar : 'a array [@bits max_window_size_bits] [@length num_windows]
      ; scalar_negatives : 'a array [@length num_windows]
      ; scalar_valid : 'a
      ; last_scalar : 'a
      ; input_point : 'a Xyt.t [@rtlprefix "o$xyt$"]
      ; scalar_and_input_point_ready : 'a
      }
    [@@deriving sexp_of, hardcaml ~rtlprefix:"o$"]
  end

  module Scalar_compute = struct
    module T = struct
      type 'a t =
        { (* actually use the correct bit widths, just pad with 0s for interface *)
          windows : 'a array
             [@bits max_window_size_bits] [@length num_windows] [@rtlprefix "windows$"]
        ; carry : 'a (* not relevant, just carried along *)
        ; last_scalar : 'a
        ; input_point : 'a Xyt.t [@rtlprefix "xyt$"]
        }
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
        ; up_scalar : 'a Scalar_compute.With_valid.t [@rtlprefix "i_up_scalar$"]
        ; dn_ready : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t =
        { dn_scalar : 'a Scalar_compute.With_valid.t [@rtlprefix "o_dn_scalar$"]
        ; up_ready : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end
  end

  module Register_stage = struct
    module I = Pipeline_stage.I
    module O = Pipeline_stage.O

    let data_bits = Int.round_up Scalar_compute.sum_of_port_widths ~to_multiple_of:8

    module Axi_wrapper = Hardcaml_axi.Make (struct
      let addr_bits = 32
      let data_bits = data_bits
    end)

    module Register = Axi_wrapper.Stream.Register

    let create scope ({ clock; clear; up_scalar; dn_ready } : _ I.t) : _ O.t =
      let up =
        { (Axi_wrapper.Stream.Source.Of_signal.of_int 0) with
          tdata = uresize (Scalar_compute.Of_signal.pack up_scalar.value) data_bits
        ; tvalid = up_scalar.valid
        }
      in
      let dn_dest = { Axi_wrapper.Stream.Dest.tready = dn_ready } in
      let register = Register.hierarchical scope { clock; clear; up; dn_dest } in
      let dn_scalar =
        { With_valid.valid = register.dn.tvalid
        ; value = Scalar_compute.Of_signal.unpack (sel_bottom register.dn.tdata data_bits)
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
      let ( -- ) = Scope.naming scope in
      Array.iteri up_scalar.value.windows ~f:(fun i v ->
        ignore (v -- Printf.sprintf "0_up$0_src$windows$w_%02d" i : Signal.t));
      ignore (up_scalar.valid -- "0_up$0_src$valid" : Signal.t);
      ignore (up_scalar.value.carry -- "0_up$0_src$carry" : Signal.t);
      ignore (dn_ready -- "1_dn$1_dst$ready" : Signal.t);
      let up_ready = wire 1 in
      let reg_with_handshake v =
        let spec = Reg_spec.create ~clock ~clear () in
        reg spec v ~enable:up_ready
      in
      assert (0 < stage && stage < num_windows);
      (* compute window and carry widths *)
      let c_prev = window_bit_sizes.(stage - 1) in
      (* first perform the carry on the previous bucket *)
      let up_prev_bucket_with_overflow =
        let v = sel_bottom up_scalar.value.windows.(stage - 1) c_prev in
        Uop.(v +: up_scalar.value.carry)
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
          Scalar_compute.With_valid.map up_scalar ~f:reg_with_handshake
        in
        let carry = reg_with_handshake (dn_prev_overflow |: dn_prev_negate) in
        let windows = registered_up.value.windows in
        windows.(stage - 1)
          <- reg_with_handshake
               (uresize
                  (mux2 dn_prev_negate dn_prev_signed_and_shifted dn_prev_bucket_unsigned)
                  max_window_size_bits);
        { registered_up with value = { registered_up.value with windows; carry } }
      in
      Array.iteri dn_scalar.value.windows ~f:(fun i v ->
        ignore (v -- Printf.sprintf "1_dn$0_src$windows$w_%02d" i : Signal.t));
      ignore (dn_scalar.value.carry -- "1_dn$0_src$carry" : Signal.t);
      ignore (dn_scalar.valid -- "1_dn$0_src$valid" : Signal.t);
      ignore (up_ready -- "0_up$1_dst$ready" : Signal.t);
      (* we can accept whenever we are currently unoccupied, or we are occupied and being drained *)
      up_ready <== (~:(dn_scalar.valid) |: (dn_scalar.valid &: dn_ready));
      { O.dn_scalar; up_ready }
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

    let _ = Register
  end

  let rec construct_pipeline ~output ~input ~clock ~clear ~scope stages up_scalar =
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
        Scalar_compute.With_valid.Of_signal.( <== ) output dn_scalar;
        input)
      else
        (* continue construting the downstream pipeline *)
        construct_pipeline ~output ~input ~scope ~clock ~clear stages_tl dn_scalar
    in
    dn_ready <== next_up_ready;
    up_ready
  ;;

  let create scope (i : _ I.t) : _ O.t =
    (*let ( -- ) = Scope.naming scope in*)
    (* construct input *)
    let input =
      { With_valid.valid = i.scalar_valid
      ; value =
          { Scalar_compute.windows =
              Array.map2_exn window_bit_sizes window_bit_offsets ~f:(fun width offset ->
                uresize i.scalar.:+[offset, Some width] max_window_size_bits)
          ; carry = gnd
          ; last_scalar = i.last_scalar
          ; input_point = i.input_point
          }
      }
    in
    (* construct pipeline *)
    let output = Scalar_compute.With_valid.Of_signal.wires () in
    let scalar_and_input_point_ready =
      construct_pipeline
        ~input:i.scalar_and_input_point_ready
        ~output
        ~scope
        ~clock:i.clock
        ~clear:i.clear
        (List.init (num_windows - 1) ~f:(fun i -> Stage.Minus_1 (i + 1)))
        input
    in
    (* construct the output *)
    let output_scalar, output_negatives =
      (* perform the final carry *)
      Array.mapi output.value.windows ~f:(fun i v ->
        let v = sel_bottom v window_bit_sizes.(i) in
        let unsigned_val, negative =
          if i = num_windows - 1
          then (* this can never overflow *)
            Uop.(v +: output.value.carry), gnd
          else (
            (* CR rayesantharao: can do a bit trick here: (n + mask ) ^ mask (mask = (sra n (w - 1))) *)
            let negative = msb v in
            let bucket = mux2 negative (negate v) v in
            bucket, negative)
        in
        uresize unsigned_val max_window_size_bits, negative)
      |> Array.unzip
    in
    { O.scalar_and_input_point_ready
    ; scalar = output_scalar
    ; scalar_negatives = output_negatives
    ; scalar_valid = output.valid
    ; last_scalar = output.value.last_scalar
    ; input_point = output.value.input_point
    }
  ;;

  let hierarchical ?instance scope =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ?instance ~name:"scalar_transformation" ~scope create
  ;;
end
