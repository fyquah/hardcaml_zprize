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
        { lhs = a
        ; rhs_list = [ { op = `Sub; term = b }; { op = `Add; term = of_z ~width p } ]
        }
    with
    | [ res0; res1 ] -> res0, res1
    | _ -> assert false
  in
  uresize (mux2 res0.carry res1.result res0.result) width
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
end
