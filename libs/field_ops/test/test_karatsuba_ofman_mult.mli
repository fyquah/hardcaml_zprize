open! Hardcaml
open! Field_ops_lib

type test_case =
  { a : Z.t
  ; b : Z.t
  }

module Make (M : sig
  val bits : int
end) : sig
  include module type of Karatsuba_ofman_mult.With_interface (M)

  val test
    :  sim:Cyclesim.With_interface(I)(O).t
    -> test_cases:test_case list
    -> config:Karatsuba_ofman_mult.Config.t
    -> unit
end

val config_single_stage : Karatsuba_ofman_mult.Config.t
val config_four_stages : Karatsuba_ofman_mult.Config.t
