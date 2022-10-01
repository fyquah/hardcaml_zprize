open Core
open Hardcaml
module C = Twisted_edwards_model_lib.Conversions

module Make (Config : Msm_pippenger.Config.S) = struct
  module Twisted_edwards = Twisted_edwards_model_lib.Twisted_edwards_curve
  module Weierstrass = Twisted_edwards_model_lib.Weierstrass_curve
  module Config_utils = Msm_pippenger.Config_utils.Make (Config)
  open Config_utils

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

  let random_inputs ?(precompute = false) ?(seed = 0) ~top_window_size num_inputs =
    Random.init seed;
    Array.init num_inputs ~f:(fun _ ->
      let affine_point =
        Ark_bls12_377_g1.(mul (subgroup_generator ()) ~by:(Random.int 100))
      in
      let affine_point = twisted_edwards_affine_of_ark_bls12_377 affine_point in
      let affine_point_with_t = Twisted_edwards.affine_to_affine_with_t affine_point in
      let affine_point_with_t_for_fpga =
        if precompute
        then
          Twisted_edwards.affine_with_t_to_host_extended_representation
            bls12_377_twisted_edwards_params
            affine_point_with_t
        else affine_point_with_t
      in
      { Msm_input.scalar =
          ((* ensure the top window is never all 1s - this is true for our actual 253-bit scalar *)
           let top =
             Random.int ((1 lsl top_window_size) - 1)
             |> Bits.of_int ~width:top_window_size
           in
           Bits.(top @: random ~width:(Config.scalar_bits - top_window_size)))
      ; affine_point_with_t =
          { x = Bits.of_z ~width:Config.field_bits affine_point_with_t_for_fpga.x
          ; y = Bits.of_z ~width:Config.field_bits affine_point_with_t_for_fpga.y
          ; t = Bits.of_z ~width:Config.field_bits affine_point_with_t_for_fpga.t
          }
      ; affine_point =
          { x = Bits.of_z ~width:Config.field_bits affine_point.x
          ; y = Bits.of_z ~width:Config.field_bits affine_point.y
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
             (double !cnt2 ~times:window_bit_offsets.(window_idx)));
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

  let twisted_edwards_extended_to_affine ?(precompute = false) ?(has_t = true) extended =
    let extended =
      if precompute
      then Twisted_edwards.from_fpga_internal_representation extended
      else extended
    in
    Twisted_edwards.extended_to_affine extended ~has_t
    |> C.twisted_edwards_affine_to_weierstrass_affine bls12_377_twisted_edwards_params
  ;;

  module Reduced_scalar = struct
    type t =
      { scalar : Bits.t
      ; negative : bool
      }
    [@@deriving sexp_of]
  end

  (*module Int_reduced_scalar = struct
    type t = {
      scalar : int
    ; negative : bool
    } [@@deriving sexp_of]

    let of_reduced_scalar { Reduced_scalar.scalar; negative } : t = 
      { scalar = Bits.to_int scalar; negative }
  end*)

  module Z = struct
    include Z

    let sexp_of_t t = Sexp.Atom ("0x" ^ Z.format "x" t)
  end

  let check_scalar_reduction scalar (reduced_scalars : Reduced_scalar.t array) =
    let v =
      Array.foldi reduced_scalars ~init:Z.zero ~f:(fun i acc v ->
        let scalar_val = Bits.to_int v.scalar in
        assert (scalar_val >= 0);
        if scalar_val > num_buckets i
        then raise_s [%message (i : int) (scalar_val : int) (num_buckets i : int)];
        let sign_ = if v.negative then Z.minus_one else Z.one in
        let base = window_bit_offsets.(i) in
        (*print_s [%message (i :int) (scalar_val : int) (sign_ : Z.t) (base : int)];*)
        let scalar_val = Z.of_int scalar_val in
        Z.(acc + (sign_ * scalar_val * (one lsl base))))
    in
    let int_scalar = Bits.to_z ~signedness:Unsigned scalar in
    if not Z.(equal v int_scalar)
    then (
      let split_scalar =
        Array.map2_exn window_bit_sizes window_bit_offsets ~f:(fun size offset ->
          Bits.(scalar.:+[offset, Some size]))
      in
      Array.iteri
        (Array.zip_exn split_scalar reduced_scalars)
        ~f:(fun i (orig, { scalar; negative }) ->
        let orig = Bits.to_int orig in
        let reduced_scalar = Bits.to_int scalar in
        print_s
          [%message
            (i : int) (orig : Int.Hex.t) (reduced_scalar : Int.Hex.t) (negative : bool)]);
      raise_s
        [%message
          (split_scalar : Bits.t array)
            (reduced_scalars : Reduced_scalar.t array)
            (v : Z.t)
            (int_scalar : Z.t)])
  ;;

  let perform_scalar_reduction scalar =
    assert (Bits.width scalar = Config.scalar_bits);
    let carry = ref false in
    let rec loop res =
      let i = List.length res in
      let size = window_bit_sizes.(i) in
      let offset = window_bit_offsets.(i) in
      (*print_s [%message (i : int) (size : int) (offset : int)];*)
      let orig_slice =
        Bits.(to_int scalar.:+[offset, Some size]) + if !carry then 1 else 0
      in
      if i = Config.num_windows - 1
      then
        { Reduced_scalar.scalar = Bits.of_int ~width:max_window_size_bits orig_slice
        ; negative = false
        }
        :: res
      else (
        let signed_val =
          if orig_slice >= 1 lsl size
          then (
            (* overflow after carry *)
            assert (orig_slice = 1 lsl size);
            carry := true;
            0)
          else if (* no overflow, check the width *)
                  orig_slice >= 1 lsl (size - 1)
          then (
            carry := true;
            orig_slice - (1 lsl size))
          else (
            carry := false;
            orig_slice)
        in
        let reduced =
          { Reduced_scalar.scalar =
              Int.abs signed_val |> Bits.of_int ~width:max_window_size_bits
          ; negative = signed_val < 0
          }
        in
        loop (reduced :: res))
    in
    let ret = loop [] |> List.rev |> Array.of_list in
    check_scalar_reduction scalar ret;
    ret
  ;;

  let convert_scalar s =
    let open Bits in
    let reduced_scalars = perform_scalar_reduction s in
    Array.map2_exn reduced_scalars window_bit_sizes ~f:(fun { scalar; negative } size ->
      let v = uresize scalar size in
      if negative then negate v else v)
    |> Array.to_list
    |> concat_lsb
  ;;
end
