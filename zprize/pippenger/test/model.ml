(* Build a model of the pipeline and required controller so we can experiment with
   different schemes.

   Our core problem is that we must compute [a = a + b], so we cannot start a new
   calculation for [a] if a current computation is currently in the adder pipeline.
   The pipeline is some 150 cycles long.
*)
open Core

let window_bits = 12
let num_windows = 7
let target_pipeline_depth = 150
let pipeline_queue_depth = (target_pipeline_depth + num_windows - 1) / num_windows

let%expect_test "" =
  print_s
    [%message
      (window_bits : int)
        (num_windows : int)
        (target_pipeline_depth : int)
        (pipeline_queue_depth : int)];
  [%expect
    {|
    ((window_bits 12) (num_windows 7) (target_pipeline_depth 150)
     (pipeline_queue_depth 22)) |}]
;;

type t =
  { in_pipeline : int Queue.t array
  ; mutable steps : int
  ; stalls : int array
  ; stalled : int Queue.t array
  ; max_stall_depth : int array
  ; scalars : int Queue.t array
  }
[@@deriving sexp_of]

module Summary = struct
  type nonrec t = t

  let sexp_of_t { steps; stalls; stalled; max_stall_depth; scalars; _ } =
    let scalars_left = Array.map scalars ~f:Queue.length in
    let stalled_lengths = Array.map stalled ~f:Queue.length in
    [%message
      (steps : int)
        (stalls : int array)
        (max_stall_depth : int array)
        (scalars_left : int array)
        (stalled_lengths : int array)]
  ;;
end

let create num_scalars =
  let in_pipeline : int Queue.t array =
    Array.init num_windows ~f:(fun _ -> Queue.create ())
  in
  let stalled = Array.init num_windows ~f:(fun _ -> Queue.create ()) in
  let scalars = Array.init num_windows ~f:(fun _ -> Queue.create ()) in
  for window = 0 to num_windows - 1 do
    for _ = 0 to pipeline_queue_depth - 1 do
      Queue.enqueue in_pipeline.(window) 0
    done;
    for _ = 0 to num_scalars - 1 do
      let scalar = 1 + Random.int ((1 lsl window_bits) - 1) in
      assert (scalar <> 0);
      Queue.enqueue scalars.(window) scalar
    done
  done;
  { in_pipeline
  ; steps = 0
  ; stalls = Array.init num_windows ~f:(Fn.const 0)
  ; stalled
  ; max_stall_depth = Array.init num_windows ~f:(Fn.const 0)
  ; scalars
  }
;;

(* Is the value at the head of the queue in the processing pipelined for the given window.
   Used for both scalar and stalled values. *)
let is_in_pipeline t ~window queue =
  let value = Queue.peek_exn queue in
  Queue.exists t.in_pipeline.(window) ~f:(Int.equal value)
;;

(* Put the given value into the pipeline *)
let add_to_pipeline t ~window value =
  ignore (Queue.dequeue_exn t.in_pipeline.(window) : int);
  Queue.enqueue t.in_pipeline.(window) value
;;

(* Put a 'bubble' into the pipeline *)
let pipeline_bubble t ~window = add_to_pipeline t ~window 0

(* Add the head of the given queue to the pipeline *)
let add_to_pipeline t ~window queue =
  let value = Queue.dequeue_exn queue in
  add_to_pipeline t ~window value
;;

(* Add the next scalar value for the given window to the stalled queue *)
let add_to_stalled t ~window =
  Queue.enqueue t.stalled.(window) (Queue.dequeue_exn t.scalars.(window))
;;

(* Are both the scalar and stalled queues all empty. *)
let queues_are_empty t =
  Array.fold t.scalars ~init:true ~f:(fun acc q -> acc && Queue.is_empty q)
  && Array.fold t.stalled ~init:true ~f:(fun acc q -> acc && Queue.is_empty q)
;;

let have_scalars t ~window = not (Queue.is_empty t.scalars.(window))
let have_stalled t ~window = not (Queue.is_empty t.stalled.(window))

let test_uneven ?(verbose = false) num_scalars =
  let t = create num_scalars in
  while not (queues_are_empty t) do
    for window = 0 to num_windows - 1 do
      if verbose && (1 + t.steps) % 100_000 = 0 then print_s [%message (t : Summary.t)];
      (* if verbose && (1 + t.steps) % 10_000_000 = 0 then raise_s [%message (t : t)]; *)
      t.steps <- t.steps + 1;
      (* if t.steps % 1_000_000 = 0 then print_s [%message (t : t)]; *)
      (* check for any stalled elements, and see if we can retire them now *)
      let stalled = t.stalled.(window) in
      let scalars = t.scalars.(window) in
      (* Check the stalled queue first. *)
      if have_stalled t ~window && not (is_in_pipeline t ~window stalled)
      then add_to_pipeline t ~window stalled
      else if (* See if we can retire the current value.  Otherwise it goes into the stalled queue. *)
              have_scalars t ~window
      then
        if not (is_in_pipeline t ~window scalars)
        then add_to_pipeline t ~window scalars
        else (
          (* We are fully stalled - record the fact and pump the pipeline *)
          t.stalls.(window) <- t.stalls.(window) + 1;
          add_to_stalled t ~window;
          pipeline_bubble t ~window)
      else (* keep pumpin' *) pipeline_bubble t ~window
    done;
    for window = 0 to num_windows - 1 do
      t.max_stall_depth.(window)
        <- max t.max_stall_depth.(window) (Queue.length t.stalled.(window))
    done
  done;
  t
;;

let%expect_test "" =
  let num_tests = 1_000_000 in
  let t = test_uneven num_tests in
  let percentage_stalled =
    let x = Float.of_int (num_tests * num_windows) in
    let y = Float.of_int t.steps in
    100. *. (y -. x) /. x
  in
  print_s [%message (t : t) (percentage_stalled : float)];
  [%expect
    {|
    ((t
      ((in_pipeline
        ((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
         (3394 1954 2725 3125 516 3707 1771 2065 3922 973 182 742 2124 3525 3596
          3668 3747 866 547 1018 2975 3129)
         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
         (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
       (steps 7037982) (stalls (5283 5317 5358 5426 5380 5341 5330))
       (stalled (() () () () () () ())) (max_stall_depth (3 3 3 3 3 4 3))
       (scalars (() () () () () () ()))))
     (percentage_stalled 0.5426)) |}]
;;

(* Do all windows currently have a stalled value. *)
let all_windows_stalled t =
  let any_empty =
    Array.fold t.stalled ~init:false ~f:(fun acc q -> acc || Queue.is_empty q)
  in
  not any_empty
;;

(* Should we process stalled values?  Currently checks if any fifo is at a certain level.
   This effectively bounds the neccessary size of the fifos. *)
let clear_stalled_queues_heuristic t =
  Array.fold t.stalled ~init:false ~f:(fun acc q -> acc || Queue.length q > 3)
;;

let test_even ?(verbose = false) num_scalars =
  let t = create num_scalars in
  while not (queues_are_empty t) do
    if all_windows_stalled t
       || (not (have_scalars t ~window:0))
       || clear_stalled_queues_heuristic t
    then
      (* If all windows have stalled coefficients, or there are no scalars left. *)
      for window = 0 to num_windows - 1 do
        if verbose && (1 + t.steps) % 100_000 = 0 then print_s [%message (t : Summary.t)];
        let stalled = t.stalled.(window) in
        t.steps <- t.steps + 1;
        if have_stalled t ~window && not (is_in_pipeline t ~window stalled)
        then add_to_pipeline t ~window stalled
        else (
          t.stalls.(window) <- t.stalls.(window) + 1;
          pipeline_bubble t ~window)
      done
    else if have_scalars t ~window:0
    then
      (* process a batch of scalars *)
      for window = 0 to num_windows - 1 do
        if verbose && (1 + t.steps) % 100_000 = 0 then print_s [%message (t : Summary.t)];
        let scalars = t.scalars.(window) in
        t.steps <- t.steps + 1;
        if not (is_in_pipeline t ~window scalars)
        then add_to_pipeline t ~window scalars
        else (
          t.stalls.(window) <- t.stalls.(window) + 1;
          add_to_stalled t ~window;
          pipeline_bubble t ~window)
      done
    else
      (* Fully stalled *)
      for window = 0 to num_windows - 1 do
        if verbose && (1 + t.steps) % 100_000 = 0 then print_s [%message (t : Summary.t)];
        t.steps <- t.steps + 1;
        t.stalls.(window) <- t.stalls.(window) + 1;
        pipeline_bubble t ~window
      done;
    for window = 0 to num_windows - 1 do
      t.max_stall_depth.(window)
        <- max t.max_stall_depth.(window) (Queue.length t.stalled.(window))
    done
  done;
  t
;;

let%expect_test "" =
  let num_tests = 1_000_000 in
  let t = test_even num_tests in
  let percentage_stalled =
    let x = Float.of_int (num_tests * num_windows) in
    let y = Float.of_int t.steps in
    100. *. (y -. x) /. x
  in
  print_s [%message (t : t) (percentage_stalled : float)];
  [%expect
    {|
    ((t
      ((in_pipeline
        ((837 2819 3422 54 4063 3889 1117 3328 659 1055 227 3209 474 3433 3778
          2009 3999 76 238 0 0 0)
         (1792 2011 3330 3235 3129 68 2729 3938 2255 1035 1856 2483 1938 1517
          3814 347 3888 2903 358 0 0 0)
         (2673 3841 70 1609 2958 9 2215 1578 1832 770 1283 2907 180 3536 948 2300
          2540 3617 351 1800 2262 3065)
         (3125 516 3707 1771 2065 3922 973 182 742 2124 3525 3596 3668 3747 866
          547 1018 2975 3129 2636 0 0)
         (2434 3302 1643 2213 1071 2982 672 1576 601 56 3312 1797 2086 3505 529
          344 534 1859 925 833 3815 0)
         (2214 2838 4029 2644 3591 1646 2584 2714 763 1361 2856 1966 1233 3889
          3263 2743 3070 1314 43 0 0 0)
         (1281 1902 0 231 934 1878 459 1334 134 843 3645 1090 3384 1110 3877 98
          3088 1210 2052 2930 2623 0)))
       (steps 7057771) (stalls (8253 8253 8253 8253 8253 8253 8253))
       (stalled (() () () () () () ())) (max_stall_depth (4 4 4 4 4 4 4))
       (scalars (() () () () () () ()))))
     (percentage_stalled 0.8253)) |}]
;;
