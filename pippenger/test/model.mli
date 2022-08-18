(** Model the adder pipeline for pippengers algorithm. *)

type t [@@deriving sexp_of]

module Summary : sig
  type nonrec t = t [@@deriving sexp_of]
end

(** This test models pippengers and allows scalars in different windows to be processed
    at different rates.

    This is a problem as we would have to queue both the scalars for each window along
    with the related affine points.

    Whenever there is a stalled value in a window we try to execute it - if we cant we
    process the input scalar.  We only store if neither of these operations can execute.

    That said, it does demonstrate about the lowest stall rate I can think to implement.
*)
val test_uneven : ?verbose:bool -> int -> t

(** This is a more practical version, which has slightely worse stall rate, but I believe
    a much simpler implementation.

    First, we check if all windows have at least 1 stalled point, or if any window has
    more than some small limit.  If so we will process the stalled points in all windows.
    Otherwise we process the input scalar across all windows.

    In this way we can process one complete scalar+affine point at a time, or otherwise
    a set of stalled values.  Because the windows are processed at an exact rate, we can
    check far fewer values in the pipeline each time (ie only those related to the current)
    window on that cycle).

    So, we roughly break things up and process all windows the same way each time. This
    keeps the scalar values aligned with the input affine points, so we dont need to buffer
    things too much.  We do need a a few more dead cycles in the pipeline when processing
    stalled values, but not so much.   By controlling the max stalled fifo size, I think we
    can significantly reduce the buffering needed at a small cost to stalls.
*)
val test_even : ?verbose:bool -> int -> t
