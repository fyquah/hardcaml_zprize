open Base
open Hardcaml
open Signal

let (<<) a b = sll a b 

let latency ~depth = 3 * depth

type m_terms =
  { m0 : Signal.t
  ; m1 : Signal.t
  ; m2 : Signal.t
  }

let rec create_recursive ~clock ~enable ~level (a : Signal.t) (b : Signal.t) =
  assert (width a = width b);
  assert (level >= 1);
  let spec = Reg_spec.create ~clock () in
  let reg x = reg spec ~enable x in
  let pipeline ~n x = pipeline ~enable spec x ~n in
  let w = width a in
  let top_half x = Signal.sel_top x (w / 2) in
  let btm_half x = Signal.sel_bottom x (w / 2) in
  let { m0; m1; m2 } =
    let a0 =
      mux2 (btm_half a >: top_half a)
        (btm_half a -: top_half a)
        (top_half a -: btm_half a)
      |> reg
    in
    let a1 =
      mux2 (top_half b >: btm_half b)
        (top_half b -: btm_half b)
        (btm_half b -: top_half b)
      |> reg
    in
    match level with
    | 1 ->
      let m0 = reg (top_half a *: top_half b) in
      let m2 = reg (btm_half a *: btm_half b) in
      let m1 = reg (a0 *: a1) in
      { m0; m1; m2 }
    | _ ->
      let m0 =
        create_recursive ~enable ~clock ~level:(level - 1)
          (top_half a)
          (top_half b)
      in
      let m2 =
        create_recursive ~enable ~clock ~level:(level - 1)
          (btm_half a)
          (btm_half b)
      in
      let m1 =
        create_recursive ~enable ~clock ~level:(level - 1)
          a0
          a1
      in
      { m0; m1; m2 }
  in
  let sign =
    pipeline
      ~n:(3 * (level - 1))
      ((btm_half a <: top_half a) ^: (top_half b <: btm_half b))
  in
  ((m0 << w)
   +: ((m0
        +: m2
        +: (mux2 sign (negate m1) m1))
       << (w / 2))
   +: m2)
  |> reg
;;

let create ?(enable = vdd) ~depth ~clock a b : Signal.t =
  create_recursive ~level:depth ~enable ~clock a b
;;
