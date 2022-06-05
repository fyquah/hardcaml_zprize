(* Based on https://github.com/ZcashFoundation/zcash-fpga/blob/c4c0ad918898084c73528ca231d025e36740d40c/ip_cores/util/src/rtl/subtracter_pipe.sv
  
   See details in adder_pipe for more information about the idea behind
   splitting to multiple cycles. Loosely, speaking the implementation below
   computes the following over multiple cycles:

   {[
     mux2 (x >: y)
       (x -: y)
       (x -: y +: p)
   ]}
*)

(* TODO(fyquah): A lot of the code here looks awfully similar to
 * adder_pipe. Consider sharing more stuff.
 *)

open Base
open Hardcaml

module Borrow_and_result = struct
  type 'a t =
    { carry : 'a
    ; borrow : 'a
    ; result : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module Subtractor_state = struct
  type 'a t =
    { with_mod : 'a Borrow_and_result.t
    ; no_mod : 'a Borrow_and_result.t
    }
  [@@deriving sexp_of, hardcaml]
end

module Subtractor_input = struct
  type 'a t =
    { x : 'a
    ; y : 'a
    ; p : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

let latency ~stages = stages

let create_stage (type a)
    (module Comb : Comb.S with type t = a)
    (state : a Subtractor_state.t)
    (input : a Subtractor_input.t) =
  let open Comb in
  let w = width input.x in
  let with_mod =
    let { Subtractor_input. x; y; p } =
      Subtractor_input.map input ~f:(fun x -> uresize x (w + 1))
    in
    let lhs = uresize state.with_mod.carry (w + 1) +: x +: p in
    let rhs = uresize state.with_mod.borrow (w + 1) +: y in
    let partial_result = lhs -: rhs in
    let carry = msb partial_result in
    let borrow = lhs <: rhs in
    let result = concat_msb_e [ lsbs partial_result; state.with_mod.result ] in
    { Borrow_and_result. borrow; result; carry }
  in
  let no_mod =
    (* [no_mod] will never required the [carry] bit, since we are never adding
     * anything.
     *)
    let x = uresize input.x (w + 1) in
    let y = Uop.(input.y +: state.no_mod.borrow) in
    let borrow = x <: y in
    let result = concat_msb_e [ uresize (x -: y) w; state.no_mod.result ] in
    { Borrow_and_result. borrow; result; carry = gnd }
  in
  { Subtractor_state. with_mod; no_mod }
;;

let create ~clock ~enable ~stages ~p (x : Signal.t) (y : Signal.t) : Signal.t =
  let open Signal in
  let wx = Signal.width x in
  let wy = Signal.width y in
  if wx <> wy then (
    raise_s [%message
       "Width mismatch of inputs in Subtractor_pipe" (wx : int) (wy : int)
    ]
  );
  assert (Signal.width p = Signal.width x);
  let w = Signal.width x in
  let stage_width = (w + (stages - 1)) / stages in
  let spec = Reg_spec.create ~clock () in
  let stage_inputs =
    { Subtractor_input. x; y; p }
    |> Subtractor_input.map ~f:(Signal.split_lsb ~exact:false ~part_width:stage_width)
    |> Subtractor_input.to_interface_list
    |> List.mapi ~f:(fun n subtractor_input ->
        Subtractor_input.map ~f:(pipeline spec ~enable ~n) subtractor_input)
  in
  let spec = Reg_spec.create ~clock () in
  let final_state =
    let init =
      let no_borrow_and_result =
        { Borrow_and_result. borrow = gnd; result = empty; carry = gnd }
      in
      { Subtractor_state.
        no_mod   = no_borrow_and_result
      ; with_mod = no_borrow_and_result
      }
    in
    List.fold stage_inputs ~init ~f:(fun stage_state stage_input ->
        create_stage (module Signal) stage_state stage_input
        |> Subtractor_state.map ~f:(reg ~enable spec))
  in
  let final_result =
    mux2 final_state.no_mod.borrow
      final_state.with_mod.result
      final_state.no_mod.result
  in
  final_result
;;

module With_interface(M : sig val bits : int end) = struct
  include M

  module I = struct
    type 'a t =
      { clock : 'a
      ; enable : 'a
      ; p : 'a [@bits bits]
      ; x : 'a [@bits bits]
      ; y : 'a [@bits bits]
      ; valid : 'a [@rtlname "in_valid"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { z : 'a [@bits bits]
      ; valid : 'a [@rtlname "out_valid"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create ~stages (_scope : Scope.t) ({ I. clock; enable; x; y; p; valid }) =
    let spec = Reg_spec.create ~clock () in
    { O.
      z = create ~clock ~enable ~stages ~p x y
    ; valid = Signal.pipeline spec ~n:(latency ~stages) ~enable valid
    }
  ;;
end

