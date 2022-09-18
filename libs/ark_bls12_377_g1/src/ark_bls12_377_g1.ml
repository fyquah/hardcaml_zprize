open Core
open Ctypes
open Foreign

let sexp_of_z z = Sexp.Atom ("0x" ^ Z.format "x" z)

module External = struct
  let potential_dl_filenames =
    (* The more principled thing to do is to figure out the build-target and
     * use .so or .dylib.. but this works well enough.
     * *)
    let%bind.List extension = [ "so"; "dylib" ] in
    let%bind.List dir =
      (* CR-someday fyquah: This is getting out of hand! refactor to keep
       * looking until root and terminate dynamically.
       *)
      [ "."
      ; "../"
      ; "../../"
      ; "../../../"
      ; "../../../.."
      ; "../../../../.."
      ; "../../../../../../"
      ; "../../../../../../../"
      ; "../../../../../../../../"
      ; "../../../../../../../../../"
      ]
    in
    [ dir ^/ "libs/rust/ark_bls12_377_g1/target/debug/libark_bls12_377_g1." ^ extension ]
  ;;

  let () =
    (* Unfortunately, We can't install Core_unix on M1 Macs, so we are stuck
     * with Caml.Sys, rather than Sys_unix, for the time-being.
     *)
    let filename =
      match List.find potential_dl_filenames ~f:Caml.Sys.file_exists with
      | Some filename -> filename
      | None ->
        failwith
          "Cannot find Rust ark_bls12_377_g1 - did you build it? Run `cargo build` in \
           rust/ark_bls12_377_g1 first."
    in
    ignore (Dl.dlopen ~filename ~flags:[ RTLD_LAZY; RTLD_GLOBAL ] : Dl.library)
  ;;

  type affine = unit ptr

  let affine : affine typ = ptr void
  let free = foreign "ark_bls12_377_g1_free" (ptr affine @-> returning void)

  let new_ =
    foreign
      "ark_bls12_377_g1_new"
      (ptr int64_t @-> ptr int64_t @-> bool @-> returning (ptr affine))
  ;;

  let add =
    foreign "ark_bls12_377_add" (ptr affine @-> ptr affine @-> returning (ptr affine))
  ;;

  let neg = foreign "ark_bls12_377_g1_neg" (ptr affine @-> returning (ptr affine))
  let mul = foreign "ark_bls12_377_mul" (ptr affine @-> int64_t @-> returning (ptr affine))

  let subgroup_generator =
    foreign "ark_bls12_377_g1_subgroup_generator" (void @-> returning (ptr affine))
  ;;

  let coeff_a = foreign "ark_bls12_377_g1_coeff_a" (ptr int64_t @-> returning void)
  let coeff_b = foreign "ark_bls12_377_g1_coeff_b" (ptr int64_t @-> returning void)
  let modulus = foreign "ark_bls12_377_g1_modulus" (ptr int64_t @-> returning void)

  let get_x =
    foreign "ark_bls12_377_g1_get_x" (ptr affine @-> ptr int64_t @-> returning void)
  ;;

  let get_y =
    foreign "ark_bls12_377_g1_get_y" (ptr affine @-> ptr int64_t @-> returning void)
  ;;

  let get_infinity =
    foreign "ark_bls12_377_g1_get_infinity" (ptr affine @-> returning bool)
  ;;

  let is_on_curve = foreign "ark_bls12_377_g1_is_on_curve" (ptr affine @-> returning bool)

  let equal =
    foreign "ark_bls12_377_g1_equal" (ptr affine @-> ptr affine @-> returning bool)
  ;;
end

type affine = External.affine ptr

let bits_of_z z =
  let arr = CArray.of_list int64_t [ 0L; 0L; 0L; 0L; 0L; 0L ] in
  let z = Z.to_bits z in
  let get_word i =
    let b o =
      let pos = (i * 8) + o in
      if pos >= String.length z
      then 0L
      else (
        let shift = o * 8 in
        Int64.(of_int (Char.to_int z.[pos]) lsl shift))
    in
    Int64.O.(b 0 lor b 1 lor b 2 lor b 3 lor b 4 lor b 5 lor b 6 lor b 7)
  in
  for i = 0 to 5 do
    CArray.unsafe_set arr i (get_word i)
  done;
  arr
;;

let create ~x ~y ~infinity =
  let x = bits_of_z x in
  let y = bits_of_z y in
  let ptr = External.new_ (CArray.start x) (CArray.start y) infinity in
  Caml.Gc.finalise External.free ptr;
  ptr
;;

let subgroup_generator () =
  let ptr = External.subgroup_generator () in
  Caml.Gc.finalise External.free ptr;
  ptr
;;

let add (a : affine) (b : affine) =
  let ptr = External.add a b in
  Caml.Gc.finalise External.free ptr;
  ptr
;;

let neg (a : affine) =
  let ptr = External.neg a in
  Caml.Gc.finalise External.free ptr;
  ptr
;;

let mul (a : affine) ~by =
  assert (Int.is_non_negative by);
  let ptr = External.mul a (Int64.of_int by) in
  Caml.Gc.finalise External.free ptr;
  ptr
;;

let buffer_to_z arr =
  String.init (8 * 6) ~f:(fun i ->
    let word = i / 8 in
    let shift = i % 8 * 8 in
    Int64.O.((CArray.get arr word lsr shift) land 0xFFL)
    |> Int64.to_int_trunc
    |> Char.of_int_exn)
  |> Z.of_bits
;;

let create_buffer () = CArray.of_list int64_t [ 0L; 0L; 0L; 0L; 0L; 0L ]

let x (t : affine) =
  let buffer = create_buffer () in
  External.get_x t (CArray.start buffer);
  buffer_to_z buffer
;;

let y (t : affine) =
  let buffer = create_buffer () in
  External.get_y t (CArray.start buffer);
  buffer_to_z buffer
;;

let is_on_curve t = External.is_on_curve t
let infinity t = External.get_infinity t
let equal_affine a b = External.equal a b

let sexp_of_affine affine =
  let x = x affine in
  let y = y affine in
  let infinity = infinity affine in
  [%message (x : z) (y : z) (infinity : bool)]
;;

let create_coeff f =
  let l =
    lazy
      (let buffer = create_buffer () in
       f (CArray.start buffer);
       buffer_to_z buffer)
  in
  fun () -> Lazy.force l
;;

let coeff_a = create_coeff External.coeff_a
let coeff_b = create_coeff External.coeff_b
let modulus = create_coeff External.modulus

let mul_wide ~part_width a ~by:b =
  let open Hardcaml in
  let scale = 1 lsl part_width in
  let b =
    Bits.split_lsb ~part_width ~exact:false b |> List.map ~f:Bits.to_int |> List.rev
  in
  let rec f acc b =
    match b with
    | [] -> Option.value_exn acc
    | b :: tl ->
      let a_by_b = mul a ~by:b in
      (match acc with
       | None -> f (Some a_by_b) tl
       | Some acc ->
         let acc = mul acc ~by:scale in
         f (Some (add acc a_by_b)) tl)
  in
  f None b
;;

module For_testing = struct
  let sexp_of_z = sexp_of_z
end
