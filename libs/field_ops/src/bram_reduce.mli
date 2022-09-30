open Hardcaml

module type Config = sig
  val p : Z.t
  val adder_stages : int
  val num_bits : int
  val error_bits : int
end

module Make (Config : Config) : sig
  val latency : int

  module I : sig
    type 'a t =
      { clock : 'a
      ; coarse_value : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t = { reduced_value : 'a } [@@deriving sexp_of, hardcaml]
  end

  val create : build_mode:Build_mode.t -> Scope.t -> Signal.t I.t -> Signal.t O.t

  val hierarchical
    :  ?build_mode:Build_mode.t
    -> ?instance:string
    -> Scope.t
    -> Circuit.With_interface(I)(O).create
end
