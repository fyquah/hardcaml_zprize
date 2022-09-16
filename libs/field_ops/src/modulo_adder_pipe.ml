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
open Signal
open Reg_with_enable

let latency ~stages = stages

let create ~clock ~enable ~stages ~(p : Z.t) (a : Signal.t) (b : Signal.t) : Signal.t =
  let width = width a in
  let res0, res1 =
    let spec = Reg_spec.create ~clock () in
    let pipe ~n x = if Signal.is_const x then x else pipeline spec ~enable ~n x in
    match
      Modulo_adder_subtractor_pipe.create
        (module Signal)
        ~stages
        ~pipe
        { lhs = gnd @: a
        ; rhs_list =
            [ { op = `Add; term = gnd @: b }
            ; { op = `Sub; term = of_z ~width:(width + 1) p }
            ]
        }
    with
    | [ res0; res1 ] -> res0, res1
    | _ -> assert false
  in
  uresize (mux2 res1.carry res0.result res1.result) width
;;

module With_interface (M : sig
  val bits : int
end) =
struct
  include M

  module I = struct
    type 'a t =
      { clock : 'a
      ; enable : 'a
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

  let create ~stages ~p (_scope : Scope.t) { I.clock; enable; x; y; valid } =
    let spec = Reg_spec.create ~clock () in
    { O.z = create ~clock ~enable ~stages ~p x y
    ; valid = Signal.pipeline spec ~n:(latency ~stages) ~enable valid
    }
  ;;

  let hierarchical ~stages ~p scope i =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical
      ~scope
      ~name:(Printf.sprintf "modulo_adder_pipe_%dbits" bits)
      (create ~stages ~p)
      i
  ;;
end

let hierarchical ~scope ~clock ~enable ~stages ~p x y =
  if Signal.width x <> Signal.width y
  then raise_s [%message "adder_pipe requires x and y to be the same width"];
  let module M =
    With_interface (struct
      let bits = Signal.width x
    end)
  in
  let { M.O.z; valid = _ } =
    M.hierarchical ~stages ~p scope { clock; enable; x; y; valid = Signal.vdd }
  in
  z
;;
