open Hardcaml

module Make (M : sig
  val bits : int
  val output_bits : int
end) : sig
  include module type of Field_ops_lib.Barrett_reduction.With_interface (M)

  val test
    :  debug:bool
    -> test_cases:Z.t list
    -> config:Field_ops_lib.Barrett_reduction.Config.t
    -> sim:Cyclesim.With_interface(I)(O).t
    -> unit
end
