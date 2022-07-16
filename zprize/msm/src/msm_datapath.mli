(** Datapath and state machine to compute the MSM.
*
*
*    This operates in two phases:
*    1. The host writes precomputed affine points into a precomputed memory
*    buffer
*    2. The host then streams memory addresses to load precomputed points from.
*
*    The state machine starts in [S_idle].
*
*    The state machine takes in a stream of [num_entries * scalar_num_bits]
*    addresses to load precomputed points from. It computes
*    mix_add(dbl(intermediate_point), precomputed_point[addr]).  In
*    [S_work_on_bit_position], the state machine pumps the relevant parameters
*    into a internal multi-SLR datapath to compute the above.  After pumping
*    all the addresses for a bit position, the state machine entres
*    [S_wait_for_bit_position_finish]. As explained by the name, the state
*    machine waits for all tasks to finish in the bit position before moving on
*    to the next bit position. This is so that any doubling fault can be
*    detected and handled.  It's possible to do some speculative execution here
*    for better performance.
*
*    In the event of a doubling fault (ie: mixed addition on two equal
*    numbers), the state machine enters [S_handle_doubling_fault]. In this
*    state, the state machine iterates through all [num_entries] intermediate
*    points and selectively doubles the points that encounted a doubling fault.
*    This is not very efficient. However, the expectation is this state should
*    be very infrequent.
*
*    After completing everything, the state machine enters [S_stream_result]
*    to stream [num_entries] jacobian points in the intermediate points buffer.
*)

open Hardcaml
open Hardcaml_axi
module Write_port_1d = Hardcaml_xilinx.Memory_builder.Write_port_1d

module State : sig
  type t =
    | S_idle
    | S_work_on_bit_position
    | S_wait_for_bit_position_finish
    | S_handle_doubling_fault
    | S_stream_result
  [@@deriving compare, enumerate, sexp_of]
end

module Make (Config : Config.S) : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; precomputed_points_addr : 'a Axi16.Stream.Source.t
      ; precomputed_points_wr : 'a Write_port_1d.M(Affine_point_or_infinity).t
      ; num_entries_minus_1 : 'a
      ; scalar_num_bits_minus_1 : 'a
      ; result_ready : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { precomputed_points_addr_dest : 'a Axi16.Stream.Dest.t
      ; result : 'a Jacobian_point_or_infinity.With_valid.t
      ; result_last : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : build_mode:Build_mode.t -> Scope.t -> Signal.t I.t -> Signal.t O.t
  val hierarchical : build_mode:Build_mode.t -> Scope.t -> Signal.t I.t -> Signal.t O.t
end
