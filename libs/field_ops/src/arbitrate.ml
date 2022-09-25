open Core
open Hardcaml
open Signal

let rec create_pipe_chain ~reg ~n x =
  assert (n >= 1);
  if n = 1 then [ x ] else x :: create_pipe_chain ~reg ~n:(n - 1) (reg x)
;;

let create_beat ~clock ~enable ~num_entries valid_in =
  let width = Int.ceil_log2 num_entries in
  (* This allows us to create a beat without needing a clear signal. *)
  mux2
    valid_in
    (zero width)
    (reg_fb (Reg_spec.create ~clock ()) ~width ~enable ~f:(fun fb ->
       mux2 valid_in (of_int ~width 1) (fb +:. 1)))
;;

let arbitrate ~enable ~clock ~valid ~f inputs =
  let num_inputs = List.length inputs in
  let beat = create_beat ~clock ~enable ~num_entries:num_inputs valid in
  let spec = Reg_spec.create ~clock () in
  let reg = Signal.reg ~enable spec in
  let pipeline = Signal.pipeline ~enable spec in
  let muxed_input = mux beat (List.mapi inputs ~f:(fun n x -> pipeline ~n x)) in
  List.rev (create_pipe_chain ~reg ~n:num_inputs (f muxed_input))
;;

let arbitrate2 ~enable ~clock ~valid ~f (ia, ib) =
  match arbitrate ~enable ~clock ~valid ~f [ ia; ib ] with
  | [ oa; ob ] -> oa, ob
  | _ -> assert false
;;
