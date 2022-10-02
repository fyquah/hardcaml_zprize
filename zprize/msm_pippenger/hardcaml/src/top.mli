(** Computes the MSM over a stream of input points and scalars.

    After processing has finished, will return a stream of [num_buckets *
    num_windows] back to the host, with buckets in high -> low order.

    [start] is pulsed high for one clock cycle to initialize the bucket RAMs.
    When input points are ready to be streamed in, [scalar_and_input_point] will
    raise high.

    After clear, this module immediately sets all bucket RAMs to the Identity
    point, and then signals a start to the controller.

    This module will then wait for input points until [last_scalar] is high, and
    when the result is ready will be outputted on [result_point]. After the last
    result point has been read the we will start to re-initialize bucket RAMs to
    Identity, and wait for new input points to be streamed in.

    Currently this top implements a fully pipelined adder, with two pippenger
    controllers running over half the windows each.
*)

open Hardcaml

module Make (Config : Config.S) : sig
  module Mixed_add : module type of Twisted_edwards_lib.Mixed_add.Make (struct
    let num_bits = Config.field_bits
  end)

  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; scalar : 'a
      ; scalar_valid : 'a
      ; last_scalar : 'a
      ; input_point : 'a Mixed_add.Xyt.t
      ; result_point_ready : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val precompute : bool
  val input_point_bits : int
  val result_point_bits : int
  val num_windows : int
  val num_result_points : int
  val max_window_size_bits : int

  module O : sig
    type 'a t =
      { result_point : 'a Mixed_add.Xyzt.t
      ; result_point_valid : 'a
      ; last_result_point : 'a
      ; scalar_and_input_point_ready : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val hierarchical
    :  ?instance:string
    -> build_mode:Build_mode.t
    -> Scope.t
    -> Signal.t Interface.Create_fn(I)(O).t
end
