open Core
open Hardcaml
module C = Twisted_edwards_model_lib.Conversions

module Make (Config : Msm_pippenger_multi_slr.Config.S) = struct
  module Twisted_edwards = Twisted_edwards_model_lib.Twisted_edwards_curve
  module Weierstrass = Twisted_edwards_model_lib.Weierstrass_curve

  let { Msm_pippenger_multi_slr.Config.field_bits; _ } = Config.t
  let scalar_bits = Msm_pippenger_multi_slr.Config.scalar_bits Config.t
  let window_size_bits = Config.t.window_size_bits

  module Affine_point = struct
    type 'a t =
      { x : 'a [@bits field_bits]
      ; y : 'a [@bits field_bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Affine_point_with_t = struct
    type 'a t =
      { x : 'a [@bits field_bits]
      ; y : 'a [@bits field_bits]
      ; t : 'a [@bits field_bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Extended = struct
    type 'a t =
      { x : 'a [@bits field_bits]
      ; y : 'a [@bits field_bits]
      ; z : 'a [@bits field_bits]
      ; t : 'a [@bits field_bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Msm_input = struct
    type 'a t =
      { scalar : 'a [@bits scalar_bits]
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

  let random_inputs ?(seed = 0) num_inputs =
    Random.init seed;
    Array.init num_inputs ~f:(fun _ ->
      let affine_point =
        Ark_bls12_377_g1.(mul (subgroup_generator ()) ~by:(Random.int 100))
      in
      let affine_point = twisted_edwards_affine_of_ark_bls12_377 affine_point in
      let affine_point_with_t = Twisted_edwards.affine_to_affine_with_t affine_point in
      { Msm_input.scalar = Bits.random ~width:scalar_bits
      ; affine_point_with_t =
          { x = Bits.of_z ~width:field_bits affine_point_with_t.x
          ; y = Bits.of_z ~width:field_bits affine_point_with_t.y
          ; t = Bits.of_z ~width:field_bits affine_point_with_t.t
          }
      ; affine_point =
          { x = Bits.of_z ~width:field_bits affine_point.x
          ; y = Bits.of_z ~width:field_bits affine_point.y
          }
      })
  ;;

  let pidentity = Ark_bls12_377_g1.create ~x:Z.zero ~y:Z.one ~infinity:true

  let double p ~times =
    let p_int = ref p in
    for _ = 0 to times - 1 do
      p_int := Ark_bls12_377_g1.mul !p_int ~by:2
    done;
    !p_int
  ;;

  let calculate_result_from_fpga (from_fpga : window_bucket_point list) =
    let sorted_by_window =
      List.group from_fpga ~break:(fun a b -> a.window <> b.window)
    in
    let total_result = ref pidentity in
    List.iteri sorted_by_window ~f:(fun window_idx window ->
      (* sum this window *)
      let cnt1 = ref pidentity in
      let cnt2 = ref pidentity in
      List.iter window ~f:(fun p ->
        match p.point with
        | None -> cnt2 := Ark_bls12_377_g1.add !cnt2 !cnt1
        | Some p ->
          let p = ark_bls12_377_of_weiertrass_affine p in
          cnt1 := Ark_bls12_377_g1.add !cnt1 p;
          cnt2 := Ark_bls12_377_g1.add !cnt2 !cnt1);
      (* add to running sum *)
      total_result
        := Ark_bls12_377_g1.add
             !total_result
             (double !cnt2 ~times:(window_size_bits * window_idx)));
    !total_result
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
