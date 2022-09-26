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
      ; input_point : 'a Xyt.t [@rtl_prefix "i_"]
      ; scalar_and_input_point_ready : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { scalar : 'a array [@bits max_window_size_bits] [@length num_windows]
      ; scalar_negatives : 'a array [@length num_windows]
      ; scalar_valid : 'a
      ; last_scalar : 'a
      ; input_point : 'a Xyt.t [@rtl_prefix "o_"]
      ; scalar_and_input_point_ready : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Pipeline_stage = struct
    module Scalar_compute = struct
      module T = struct
        type 'a t =
          { windows : 'a array [@length num_windows]
          ; carry : 'a (* not relevant, just carried along *)
          ; last_scalar : 'a
          ; input_point : 'a Xyt.t
          }
        [@@deriving sexp_of, hardcaml]
      end

      include T
      module With_valid = With_valid.Wrap.Make (T)
    end

    module I = struct
      type 'a t =
        { up_scalar : 'a Scalar_compute.With_valid.t
        ; dn_ready : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t =
        { dn_scalar : 'a Scalar_compute.With_valid.t
        ; up_ready : 'a
        }
    end
    [@@deriving sexp_of, hardcaml]

    let create _scope ~clock ~clear ~stage ({ up_scalar; dn_ready } : _ I.t) : _ O.t =
      let up_ready = wire 1 in
      let reg_with_handshake v =
        let spec = Reg_spec.create ~clock ~clear () in
        reg spec v ~enable:up_ready
      in
      assert (0 < stage && stage < num_windows);
      (* compute window and carry widths *)
      let c_prev = window_bit_sizes.(stage - 1) in
      let c_cur = window_bit_sizes.(stage) in
      let up_prev_carry_value =
        let prev_carry_width =
          if stage = 1 then 1 else window_bit_sizes.(stage - 2) - c_prev
        in
        sll up_scalar.value.carry prev_carry_width
      in
      (* first perform the carry on the previous bucket *)
      let up_prev_bucket_with_overflow =
        Uop.(up_scalar.value.windows.(stage - 1) +: up_prev_carry_value)
      in
      assert (width up_prev_bucket_with_overflow = c_prev + 1);
      (* only a 1 bit overflow *)
      let dn_prev_overflow, dn_prev_bucket_unsigned =
        msb up_prev_bucket_with_overflow, lsbs up_prev_bucket_with_overflow
      in
      (* if x_{i-1} >= 2^{c-1}, then use (x_{c_1} - 2^c) instead 
       * assume that carry is small compared to window, so we never have to both overflow and negate
       *)
      let dn_prev_negate = msb dn_prev_bucket_unsigned in
      let dn_prev_signed_and_shifted =
        let shift_value = of_int (1 lsl c_prev) ~width:(c_cur + 1) in
        sel_bottom Sop.(dn_prev_bucket_unsigned -: shift_value) c_prev
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
               (mux2 dn_prev_negate dn_prev_signed_and_shifted dn_prev_bucket_unsigned);
        { registered_up with value = { registered_up.value with windows; carry } }
      in
      (* we can accept whenever we are currently unoccupied, or we are occupied and being drained *)
      up_ready <== (~:(dn_scalar.valid) |: (dn_scalar.valid &: dn_ready));
      { O.dn_scalar; up_ready }
    ;;
  end

  let create scope (i : _ I.t) : _ O.t =
    (*let ( -- ) = Scope.naming scope in*)
    (* construct input *)
    let input =
      { With_valid.valid = i.scalar_valid
      ; value =
          { Pipeline_stage.Scalar_compute.windows =
              Array.map2_exn window_bit_sizes window_bit_offsets ~f:(fun width offset ->
                i.scalar.:+[offset, Some width])
          ; carry = gnd
          ; last_scalar = i.last_scalar
          ; input_point = i.input_point
          }
      }
    in
    (* construct pipeline *)
    let output = Pipeline_stage.Scalar_compute.With_valid.Of_signal.wires () in
    let rec construct_pipeline stage up_scalar =
      let dn_ready = wire 1 in
      let cur_stage =
        Pipeline_stage.create
          scope
          ~clock:i.clock
          ~clear:i.clear
          ~stage
          { up_scalar; dn_ready }
      in
      let next_up_ready =
        if stage = num_windows - 1
        then (
          Pipeline_stage.Scalar_compute.With_valid.Of_signal.( <== )
            output
            cur_stage.dn_scalar;
          i.scalar_and_input_point_ready)
        else
          (* continue construting the downstream pipeline *)
          construct_pipeline (stage + 1) cur_stage.dn_scalar
      in
      dn_ready <== next_up_ready;
      cur_stage.up_ready
    in
    let scalar_and_input_point_ready = construct_pipeline 1 input in
    (* construct the output *)
    let output_scalar, output_negatives =
      (* perform the final carry *)
      Array.mapi output.value.windows ~f:(fun i v ->
        let signed_val =
          if i = num_windows - 1
          then (
            (* this can never overflow *)
            let final_carry_value =
              1 lsl (window_bit_sizes.(i - 1) - window_bit_sizes.(i))
            in
            v +:. final_carry_value)
          else v
        in
        let negative = msb signed_val in
        let unsigned_val = mux2 negative (negate signed_val) signed_val in
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
