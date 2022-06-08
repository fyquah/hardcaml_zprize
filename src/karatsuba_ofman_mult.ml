(** Implementation of the karatsuba multiplication algorithm. Highly based on
    https://github.com/ZcashFoundation/zcash-fpga/blob/c4c0ad918898084c73528ca231d025e36740d40c/ip_cores/util/src/rtl/adder_pipe.sv

    See https://en.wikipedia.org/wiki/Karatsuba_algorithm for more details for
    the algorithm.

    The algorithm expresses the two numbers to be multiplied as:

    x = 2^(w/2) * x0 + x1
    y = 2^(w/2) * y0 + y1

    In every recursive step, B=2, and w = width of the inputs. It is required
    that width a = width b, and is an even number.

    Naively expanding the terms above yields the following:

    x * y = z0 * 2^(w)
            + z1 * 2^(w/2)
            + z2 

    where

    z0 = x0*y0
    z1 = x0*y1 + x1*y0
    z2 = x1*y1

    We can express z1 as follows: (Note that this is a slightly different
    formulation from those available in wikipedia)

    z1 = (x0 - x1)(y1 - y0) + x0*y0 + x1*y1
       = (x0 - x1)(y1 - y0) + z0 + z2

    These intermediate multiplication results will be refered to m{0,1,2}:

    m0 = z0
    m1 = (x0 - x1)(y1 - y0)
    m2 = z2

    The result of [x * y] can be computed by summing out the [m0, m1 and m2]
    terms as follows:

    m0 * 2^w
    + (m0 + m2 + m1) * 2^(w/2)
    + m2
*)

open Base
open Hardcaml
open Signal

let (<<) a b = sll a b 

let latency ~depth = 2 * depth + 1

type m_terms =
  { m0 : Signal.t
  ; m1 : Signal.t
  ; m2 : Signal.t
  }

let rec create_recursive ~scope ~clock ~enable ~level (a : Signal.t) (b : Signal.t) =
  let (--) = Scope.naming scope in
  let a = a -- "in_a" in
  let b = b -- "in_b" in
  let wa = width a in
  let wb = width b in
  if wa <> wb then (
    raise_s [%message
      "Width of [a] and [b] argument of karatsuba ofman multiplier mismatch"
        (wa : int)
        (wb : int)
    ]
  );
  assert (level >= 1);
  let spec = Reg_spec.create ~clock () in
  let reg x = reg spec ~enable x in
  let pipeline ~n x = pipeline ~enable spec x ~n in
  let hw = (width a + 1) / 2 in
  let w = hw * 2 in
  let top_half x =
    (* [uresize] here is necessary to handle cases where [wa] is odd. *)
    uresize (drop_bottom x hw) hw
  in
  let btm_half x = Signal.sel_bottom x hw in
  let a' = reg a in
  let b' = reg b in
  let sign =
    pipeline
      ~n:(2 * level)
      (((btm_half a -- "btm_a") <: (top_half a -- "top_a"))
       ^: ((top_half b -- "top_b") <: (btm_half b -- "btm_b")))
  in
  let { m0; m1; m2 } =
    let a0 =
      mux2 (btm_half a >: top_half a)
        (btm_half a -: top_half a)
        (top_half a -: btm_half a)
      |> reg
      |> Fn.flip (Scope.naming scope) "a0"
    in
    let a1 =
      mux2 (top_half b >: btm_half b)
        (top_half b -: btm_half b)
        (btm_half b -: top_half b)
      |> reg
      |> Fn.flip (Scope.naming scope) "a1"
    in
    match level with
    | 1 ->
      let m0 = Scope.naming scope (reg (top_half a' *: top_half b')) "m0" in
      let m2 = Scope.naming scope (reg (btm_half a' *: btm_half b')) "m2" in
      let m1 = Scope.naming scope (reg (a0 *: a1)) "m1" in
      { m0; m1; m2 }
    | _ ->
      let recurse subscope x y =
        let scope = Scope.sub_scope scope subscope in
        create_recursive ~scope ~enable ~clock ~level:(level - 1) x y
      in
      let m0 = recurse "m0" (top_half a') (top_half b') in
      let m2 = recurse "m2" (btm_half a') (btm_half b') in
      let m1 = recurse "m1" a0 a1 in
      { m0; m1; m2 }
  in
  let m0 = uresize m0 (w * 2) in
  let m1 = uresize m1 (w * 2) in
  let m2 = uresize m2 (w * 2) in
  ((m0 << w)
   +: ((m0
        +: m2
        +: (mux2 sign (negate m1) m1))
       << hw)
   +: m2)
  |> Fn.flip uresize (2 * wa)
  |> reg
  |> Fn.flip (--) "out"
;;

let create ?(enable = vdd) ~depth ~scope ~clock a b : Signal.t =
  create_recursive ~level:depth ~scope ~enable ~clock a b
;;

module With_interface(M : sig
    val num_bits : int
    val depth : int
  end) = struct
  open M

  module I = struct
    type 'a t =
      { clock : 'a
      ; enable : 'a
      ; valid : 'a [@rtlname "in_valid"]
      ; a : 'a [@bits num_bits]
      ; b : 'a [@bits num_bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { c : 'a [@bits num_bits * 2]
      ; valid : 'a [@rtlname "out_valid"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create (scope : Scope.t) { I. clock; enable; a; b; valid }  =
    { O.c = create ~clock ~enable ~depth ~scope a b
    ; valid =
        pipeline ~n:(latency ~depth) (Reg_spec.create ~clock ()) ~enable valid
    }
  ;;

  let hierarchical scope (input : _ I.t) : _ O.t =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical
      ~scope
      ~name:(Printf.sprintf "karatsuba_ofman_mult_%d_depth_%d" num_bits depth)
      create
      input
  ;;
end

let hierarchical ~enable ~depth ~scope ~clock a b =
  let num_bits = Signal.width a in
  let module M = With_interface(struct
      let num_bits = num_bits
      let depth = depth
    end)
  in
  let o =
    M.hierarchical scope
      { clock
      ; enable
      ; valid = vdd
      ; a
      ; b
      }
  in
  o.c
;;
