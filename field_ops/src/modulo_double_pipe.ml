open Base
open Hardcaml

module Stage_state = struct
  type 'a t =
    { result : 'a
    ; borrow : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module Stage_input = struct
  type 'a t =
    { a_times_2 : 'a
    ; p : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

(* Every stage computes the partial result incorporating the previous stage's
 * borrow and producing an output borrow.
 *)
let create_stage
    (type a)
    (module Comb : Comb.S with type t = a)
    (state : a Stage_state.t)
    (input : a Stage_input.t)
  =
  let open Comb in
  assert (width input.p = width input.a_times_2);
  let w = width input.a_times_2 in
  let a_minus_p =
    uresize input.a_times_2 (w + 1)
    -: uresize input.p (w + 1)
    -: uresize state.borrow (w + 1)
  in
  { Stage_state.result = concat_msb_e [ lsbs a_minus_p; state.result ]
  ; borrow = msb a_minus_p
  }
;;

let latency ~stages = stages

let create ~stages ~p ~clock ~enable (a : Signal.t) : Signal.t =
  let open Signal in
  let w = Signal.width a in
  let p = Signal.of_z ~width:w p in
  let part_width = (w + 1 + (stages - 1)) / stages in
  let spec = Reg_spec.create ~clock () in
  let stage_inputs =
    { Stage_input.a_times_2 = a @: gnd; p = gnd @: p }
    |> Stage_input.map ~f:(Signal.split_lsb ~exact:false ~part_width)
    |> Stage_input.to_interface_list
    |> List.mapi ~f:(fun n { Stage_input.a_times_2; p } ->
           (* Pipeline the n-th chunk by [n] cycles to align it appropriately
            * to the right clock cycle of the state stage.
            *)
           { Stage_input.a_times_2 =
               pipeline spec ~enable ~n a_times_2
               (* p doesn't need to be pipelined, since it's a constant. *)
           ; p
           })
  in
  let final_stage_state =
    let init = { Stage_state.result = empty; borrow = gnd } in
    List.fold ~init stage_inputs ~f:(fun stage_state stage_input ->
        create_stage (module Signal) stage_state stage_input
        |> Stage_state.map ~f:(Signal.reg spec ~enable))
  in
  (* If there is a borrow when computing [2a - p], then simply use (a << 1)
   * as the result.
   *)
  assert (width final_stage_state.result = w + 1);
  mux2
    final_stage_state.borrow
    (pipeline spec ~enable ~n:stages (sll a 1))
    (lsbs final_stage_state.result)
;;

module With_interface (M : sig
  val bits : int
end) =
struct
  open M

  module I = struct
    type 'a t =
      { clock : 'a
      ; enable : 'a
      ; x : 'a [@bits bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { y : 'a [@bits bits] } [@@deriving sexp_of, hardcaml]
  end

  let create ~stages ~p _scope ({ clock; enable; x } : _ I.t) =
    let y = create ~stages ~p ~clock ~enable x in
    { O.y }
  ;;

  let hierarchical ~stages ~p scope (i : _ I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.create
      ~scope
      ~name:(Printf.sprintf "modulo_double_pipe_%dbits" bits)
      (create ~stages ~p)
      i
  ;;
end

let hierarchical ~stages ~p ~scope ~clock ~enable x =
  let module M =
    With_interface (struct
      let bits = Signal.width x
    end)
  in
  let { M.O.y } = M.hierarchical ~stages ~p scope { clock; enable; x } in
  y
;;
