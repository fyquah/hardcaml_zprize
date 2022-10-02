open Hardcaml

module Make (Config : Msm_pippenger.Config.S) : sig
  module Twisted_edwards = Twisted_edwards_model_lib.Twisted_edwards_curve
  module Weierstrass = Twisted_edwards_model_lib.Weierstrass_curve

  module Affine_point : sig
    type 'a t =
      { x : 'a
      ; y : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Affine_point_with_t : sig
    type 'a t =
      { x : 'a
      ; y : 'a
      ; t : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Msm_input : sig
    type 'a t =
      { scalar : 'a
      ; affine_point_with_t : 'a Affine_point_with_t.t
      ; affine_point : 'a Affine_point.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Extended : sig
    type 'a t =
      { x : 'a
      ; y : 'a
      ; z : 'a
      ; t : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  type window_bucket_point =
    { point : Weierstrass.affine option
    ; bucket : int
    ; window : int
    }
  [@@deriving sexp_of]

  val bls12_377_twisted_edwards_params : Twisted_edwards.params

  val random_inputs
    :  ?precompute:bool
    -> ?seed:int
    -> top_window_size:int
    -> int
    -> Bits.t Msm_input.t array

  val calculate_result_from_fpga : window_bucket_point list -> Ark_bls12_377_g1.affine
  val expected : Bits.t Msm_input.t array -> Ark_bls12_377_g1.affine

  val twisted_edwards_extended_to_affine
    :  ?precompute:bool
    -> ?has_t:bool
    -> Twisted_edwards.extended
    -> Weierstrass.affine option

  module Reduced_scalar : sig
    type t =
      { scalar : Bits.t
      ; negative : bool
      }
    [@@deriving sexp_of]
  end

  val perform_scalar_reduction : Bits.t -> Reduced_scalar.t array
  val check_scalar_reduction : Bits.t -> Reduced_scalar.t array -> unit
  val convert_scalar : Bits.t -> Bits.t
end
