open Core
open Hardcaml
open Msm_pippenger
module Config = Config.Bls12_377

module Scalar_transformation =
  Scalar_transformation.Make
    (Config)
    (struct
      let num_bits = Config.field_bits
    end)

open Config_utils.Make (Config)

include struct
  open Msm_pippenger_test_top.Utils.Make (Config)

  let model = perform_scalar_reduction
end

module Sim = Cyclesim.With_interface (Scalar_transformation.I) (Scalar_transformation.O)
module Waveform = Hardcaml_waveterm.Waveform

let _create_sim () =
  let scope = Scope.create ~flatten_design:true () in
  Sim.create ~config:Cyclesim.Config.trace_all (Scalar_transformation.create scope)
;;

let test_scalars =
  List.init
    (1 lsl window_bit_sizes.(0))
    ~f:(fun i ->
      let base_scalar = Bits.of_int ~width:window_bit_sizes.(0) i in
      List.init 100 ~f:(fun _ ->
        let top_width = window_bit_sizes.(num_windows - 1) in
        let top =
          Random.int_incl 0 ((1 lsl top_width) - 2) |> Bits.of_int ~width:top_width
        in
        Bits.(
          top
          @: random ~width:(Config.scalar_bits - window_bit_sizes.(0) - top_width)
          @: base_scalar)))
  |> List.concat
;;

let%expect_test "Check some scalars with the model" =
  List.iter test_scalars ~f:(fun s ->
    let _res = model s in
    ());
  print_s [%message "Checked" (List.length test_scalars : int)];
  [%expect]
;;
