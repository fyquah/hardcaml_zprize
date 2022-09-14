open Core
open Hardcaml
module C = Twisted_edwards_model_lib.Conversions

module Make (Config : Msm_pippenger.Config.S) = struct
  module Twisted_edwards = Twisted_edwards_model_lib.Twisted_edwards_curve
  module Weierstrass = Twisted_edwards_model_lib.Weierstrass_curve

  module Affine_point = struct
    type 'a t =
      { x : 'a [@bits Config.field_bits]
      ; y : 'a [@bits Config.field_bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Affine_point_with_t = struct
    type 'a t =
      { x : 'a [@bits Config.field_bits]
      ; y : 'a [@bits Config.field_bits]
      ; t : 'a [@bits Config.field_bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Extended = struct
    type 'a t =
      { x : 'a [@bits Config.field_bits]
      ; y : 'a [@bits Config.field_bits]
      ; z : 'a [@bits Config.field_bits]
      ; t : 'a [@bits Config.field_bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Msm_input = struct
    type 'a t =
      { scalar : 'a [@bits Config.scalar_bits]
      ; affine_point_with_t : 'a Affine_point_with_t.t
      ; affine_point : 'a Affine_point.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  type window_bucket_point =
    { point : Weierstrass.affine option
    ; bucket : int
    ; window : int
    }
  [@@deriving sexp_of]

  let bls12_377_weierstrass_params =
    Lazy.force Twisted_edwards_model_lib.Bls12_377_params.weierstrass
  ;;

  let bls12_377_twisted_edwards_params =
    Lazy.force Twisted_edwards_model_lib.Bls12_377_params.twisted_edwards
  ;;

  let weiertrass_affine_of_ark_bls12_377 affine_point =
    { Twisted_edwards_model_lib.Weierstrass_curve.x = Ark_bls12_377_g1.x affine_point
    ; y = Ark_bls12_377_g1.y affine_point
    }
  ;;

  let ark_bls12_377_of_weiertrass_affine
    (affine_point : Twisted_edwards_model_lib.Weierstrass_curve.affine)
    =
    Ark_bls12_377_g1.create ~x:affine_point.x ~y:affine_point.y ~infinity:false
  ;;

  let twisted_edwards_affine_of_ark_bls12_377 affine_point =
    let module C = Twisted_edwards_model_lib.Conversions in
    weiertrass_affine_of_ark_bls12_377 affine_point
    |> C.weierstrass_affine_to_twisted_edwards_affine bls12_377_weierstrass_params
  ;;

  let ark_bls12_377_of_twisted_edwards_affine affine_point =
    let module C = Twisted_edwards_model_lib.Conversions in
    let (affine : Weierstrass.affine option) =
      C.twisted_edwards_affine_to_weierstrass_affine
        bls12_377_twisted_edwards_params
        affine_point
    in
    match affine with
    | None -> Ark_bls12_377_g1.create ~x:Z.zero ~y:Z.zero ~infinity:true
    | Some p -> Ark_bls12_377_g1.create ~x:p.x ~y:p.y ~infinity:false
  ;;

  let random_inputs num_inputs =
    Random.init 0;
    Array.init num_inputs ~f:(fun _ ->
      let affine_point =
        Ark_bls12_377_g1.(mul (subgroup_generator ()) ~by:(Random.int 100))
      in
      let affine_point = twisted_edwards_affine_of_ark_bls12_377 affine_point in
      let affine_point_with_t = Twisted_edwards.affine_to_affine_with_t affine_point in
      { Msm_input.scalar = Bits.random ~width:Config.scalar_bits
      ; affine_point_with_t =
          { x = Bits.of_z ~width:Config.field_bits affine_point_with_t.x
          ; y = Bits.of_z ~width:Config.field_bits affine_point_with_t.y
          ; t = Bits.of_z ~width:Config.field_bits affine_point_with_t.t
          }
      ; affine_point =
          { x = Bits.of_z ~width:Config.field_bits affine_point.x
          ; y = Bits.of_z ~width:Config.field_bits affine_point.y
          }
      })
  ;;

  let pidentity = Ark_bls12_377_g1.create ~x:Z.zero ~y:Z.one ~infinity:true

  (* Does the slow calculation. *)
  let calculate_result_from_fpga (from_fpga : window_bucket_point list) =
    let result = ref pidentity in
    let double p ~times =
      let p_int = ref p in
      for _ = 0 to times - 1 do
        p_int := Ark_bls12_377_g1.mul !p_int ~by:2
      done;
      !p_int
    in
    List.iter from_fpga ~f:(fun p ->
      let point =
        match p.point with
        | Some p -> ark_bls12_377_of_weiertrass_affine p
        | None -> pidentity
      in
      result
        := Ark_bls12_377_g1.(
             mul point ~by:p.bucket
             |> double ~times:(Config.window_size_bits * p.window)
             |> add !result));
    !result
  ;;

  let expected (points : Bits.t Msm_input.t array) =
    Array.fold points ~init:pidentity ~f:(fun acc i ->
      let point =
        ({ x = Bits.to_z i.affine_point.x ~signedness:Unsigned
         ; y = Bits.to_z i.affine_point.y ~signedness:Unsigned
         }
          : Twisted_edwards.affine)
        |> ark_bls12_377_of_twisted_edwards_affine
      in
      Ark_bls12_377_g1.(mul_wide ~part_width:61 point ~by:i.scalar |> add acc))
  ;;

  let twisted_edwards_extended_to_affine extended =
    Twisted_edwards.extended_to_affine extended
    |> C.twisted_edwards_affine_to_weierstrass_affine bls12_377_twisted_edwards_params
  ;;
end
