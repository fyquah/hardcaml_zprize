(** After reset, takes in a stream of scalars and affine points to compute MSM
    on.

    After processing has finished, will return a stream of [num_buckets *
    num_windows] back to the host, with buckets in high -> low order.

    Currently only supports a single controller, but later will be modified to
    support a multi-SLR parallel solution.
*)
