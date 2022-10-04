(** Fully/Half pipelined mixed addition for a scaled {{:https://en.wikipedia.org/wiki/Twisted_Edwards_curve} twisted edwards curve} curve (ie: [a = -1]), optimized with precomputation.

    This implementation is designed such that one of the points is a running
    sum computed on the FPGA, and the other point is a relatively static
    affine point streamed from the host.

    This is useful when the affine points are added into the
    running sum for many many times before needing to convert back to the
    original coordinate system.

    {1 Precomputation Optimization}

    The key idea is to transform the two points (one in extended coordinate system,
    and another in affine coordinates) we want to add into an entirely different
    coordinate system such that we can skip some computation.

    The running sum in the FPGA, which is usually represented in
    {{:https://hyperelliptic.org/EFD/g1p/auto-twisted-extended-1.html} extended
    coordinate system}, is represented as follows

    {[
      running_sum_representaiton(x, y, z, t) = ( 2(y-x), 2(y+x), 4z, t )
    ]}

    Transforming the [running_sum] back to extended coordinate system is simply
    the following:

    {[
      running_sum_to_extended(p, q, r, s) = ( (p-q)/4, (p+q)/4, r/4, s )
    ]}

    Note that this means that the identity element in this coordinate system
    is no longer just [(0, 1, 1, 0)], but rather [(2, 2, 4, 0)]. Also note that
    [x/z * y/z != t/z] in the new coordinate system.

    The relative static point has a completely different representation:

    {[
      relatively_static_point_representation(x,y,t) =( (y-x)/2, (y+x)/2, 4*d*t )
    ]}

    Simliar to the running sum's internal representation, [x * y != t] in the
    new coordinate system. Unlike running sum, we will never convert this back
    to the projective coordinates. It's main purpose to be converted into this
    coordinate system is to be added into the running sum efficiently.

    Addition between these two newly defined coordinate system is as follows:

    {[
      let add_unified_with_precompute running_sum static_point =
        (* [x], [y], [z] and [t] is in the running sum representation
           coordinate system.
        *)
        let { x; y; t; z } = running_sum in
        (* [x_host], [y_host] and [t_host] is in the static point
           representation cooridnate system.
        *)
        let { x_host; y_host; t_host } = static_point in
        let c_A    = x1 * x_host in
        let c_B    = y1 * y_host in
        let c_C    = t1 * t_host in
        let c_D    = z1 in
        let c_E    = c_B - c_A in
        let c_F    = c_D - c_C in
        let c_G    = c_D + c_C in
        let c_H    = c_B + c_A in
        let pre_x3 = c_E * c_F in
        let pre_y3 = c_G * c_H in
        let t3     = c_E * c_H in
        let z3     = c_F * c_G in
        let x3     = pre_y3 - pre_x3 in
        let y3     = pre_y3 + pre_x3 in
        { x = x3; y = y3; z = z3; t = t3 }
      ;;
    ]}

    The exact proof of that this is equivalent to the
    {{:https://hyperelliptic.org/EFD/g1p/auto-twisted-extended-1.html#addition-madd-2008-hwcd-3}
    vanilla mixed addition formulae} after the transformations is beyond the
    scope of this document, as it is too large to fit in the margin of our
    screen.

    This mixed adder uses 1 less resources than the vanilla addition formulae.
    as requires 7M and 6A (over [7M + 8A + 1*2 + 1*k]) at the cost of some
    preprocessing and post-processing.

    {1 Implementation Details}

    Our implementation supports a fully-pipelined multiplier (one that can
    accept input every cycle) or a half-pipelined one (one that can accept
    input every two cycles). The latter is less resource efficient, as it
    uses 4 multipliers.

    The configuration of the adder can be specified via the {!Config.t} type.
    Most notably, thie module instantiates every stage as a hierarchical
    module. The [slr_assignments] field in the config allows the user to
    specify which SLR every stage should be mapped to. Under the hood, this
    does two things:

    - inserts a SLR\{0,1,2\} suffix to the instantiation name, in which the user
    can use for pblocking in placement)
    - automatically insert registers between stages which are not in the same
    SLR. These registers are hierachically created with SLR\{0,1,2\} suffix in
    their instantiation name so they can be pblocked.
*)

include Adder_intf.S (** @inline *)
