(** After reset, takes in a stream of scalars and affine points to compute MSM
    on.

After processing has finished, will return a stream of [num_buckets *
num_windws] back to the host.
*)
