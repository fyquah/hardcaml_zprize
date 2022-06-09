(* Based on the following implementation:
-    https://github.com/ZcashFoundation/zcash-fpga/blob/c4c0ad918898084c73528ca231d025e36740d40c/ip_cores/util/src/rtl/adder_pipe.sv

   The implementation roughly implements the following, but pipelined across
   multiple clock cycles.

   {[
     mux2 ((a + b) >= P)
       ((a + b) - P)
       (a + b)
   ]}
*)

open Base
open Hardcaml

module Adder_state = struct
  type 'a t =
    { result0 : 'a
    ; result0_carry : 'a
    ; result1 : 'a
    ; result1_carry : 'a
    ; carry_neg : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module Adder_input = struct
  type 'a t =
    { a : 'a
    ; b : 'a
    ; p : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

let create_adder_stage (type a)
    (module Comb : Comb.S with type t = a)
    (state : a Adder_state.t)
    (input : a Adder_input.t) =
  let open Comb in
  assert (width input.a = width input.b);
  assert (width input.p = width input.b);
  let w = width input.a in
  let add_res0 =
       uresize input.a (w + 1)
    +: uresize input.b (w + 1)
    +: uresize state.result0_carry (w + 1)
  in
  let add_res0_ =
       uresize input.a (w + 1)
    +: uresize input.b (w + 1)
    +: uresize state.result1_carry (w + 1)
  in
  assert (width add_res0 = w + 1);
  let carry_neg = add_res0_ <: Uop.(input.p +: state.carry_neg) in
  let add_res1 =
    let p = uresize input.p (w + 1) in
    let borrow =
      (* Equivalent to 2^w = 1 << w *)
      (carry_neg @: zero w)
    in
    add_res0_ -: p +: borrow -: uresize state.carry_neg (w + 1)
  in
  assert (width add_res1 = w + 1);
  { Adder_state.
    result0 = concat_msb_e [ lsbs add_res0; state.result0 ]
  ; result0_carry = msb add_res0
  ; result1 = concat_msb_e [ lsbs add_res1; state.result1 ]
  ; result1_carry = msb add_res1
  ; carry_neg
  }
;;

let latency ~stages = stages

let create ~clock ~enable ~stages ~p (a : Signal.t) (b : Signal.t) : Signal.t =
  let open Signal in
  assert (Signal.width a = Signal.width b);
  assert (Signal.width p = Signal.width a);
  let w = Signal.width a in
  let adder_stage_width = (w + (stages - 1)) / stages in
  let spec = Reg_spec.create ~clock () in
  let adder_inputs =
    { Adder_input. a; b; p }
    |> Adder_input.map ~f:(Signal.split_lsb ~exact:false ~part_width:adder_stage_width)
    |> Adder_input.to_interface_list
    |> List.mapi ~f:(fun n adder_input ->
        (* Pipeline the n-th chunk by [n] cycles to align it appropriately
         * to the right clock cycle of the adder stage.
         * *)
        Adder_input.map ~f:(pipeline spec ~enable ~n) adder_input)
  in
  let final_adder_state =
    let init =
      { Adder_state.
        result0 = empty
      ; result0_carry = gnd
      ; result1 = empty
      ; result1_carry = gnd
      ; carry_neg = gnd
      }
    in
    List.fold ~init adder_inputs ~f:(fun adder_state adder_input ->
        create_adder_stage (module Signal) adder_state adder_input
        |> Adder_state.map ~f:(Signal.reg spec ~enable))
  in
  mux2 final_adder_state.carry_neg
    final_adder_state.result0
    final_adder_state.result1
  |> Fn.flip uresize w
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

  let hierarchical ~stages scope i =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical
      ~scope
      ~name:(Printf.sprintf "modulo_adder_pipe_%dbits" bits)
      (create ~stages)
      i
  ;;
end

let hierarchical ~scope ~clock ~enable ~stages ~p x y =
  if Signal.width x <> Signal.width y then (
    raise_s [%message
       "adder_pipe requires x and y to be the same width"
    ]
  );
  let module M = With_interface(struct let bits = Signal.width x end) in
  let { M.O. z; valid = _ } =
    M.hierarchical ~stages scope
      { clock
      ; enable
      ; p
      ; x
      ; y
      ; valid = Signal.vdd
      }
  in
  z

;;
