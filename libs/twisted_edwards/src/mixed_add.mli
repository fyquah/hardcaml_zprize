(** Fully pipelined/half-pipelined mixed addition for twisted edwards curve with [a = -1]

    This module implements an strongly unified mixed adder for twisted edwards
    curve with [a = -1] in extended coodrinates. This means
    - the second operand must be an affine point
    - the adder doesn't need to check if the two points are identical in
      affine space.
    - the adder doesn't need to special-case identities.
    - the adder requires extended coordinates (ie: {!Xyzt.t} and {!Xyt.t})
      as input operands

    This requires 7M, 8A and 1*k (where k = 2d)

    This implementation impements the formulae specified in
    https://hyperelliptic.org/EFD/g1p/auto-twisted-extended-1.html#addition-madd-2008-hwcd-3

    See {!Mixed_add_precompute} for an implementation which uses less resources
    with some precomputation.
 *)

include Adder_intf.S (** @inline *)
