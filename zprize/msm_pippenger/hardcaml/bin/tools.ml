open Core
open Hardcaml

let command_test_vectors =
  Command.basic
    ~summary:
      "Generate input and output points that can be read by AWS host test harnesses"
    [%map_open.Command
      let num_points =
        flag "-num-points" (required int) ~doc:" The number of points to simulate over"
      and input_filename =
        flag "-input-filename" (required string) ~doc:" File to write input points to"
      and output_filename =
        flag
          "-output-filename"
          (required string)
          ~doc:" File to write expected output points to"
      and scalar_bits_arg =
        flag
          "-scalar-bits"
          (optional int)
          ~doc:
            " Override the number of scalar bits used in the algorithm, to simulate a \
             smaller number of window RAMs"
      and window_bits_arg =
        flag
          "-window-bits"
          (optional int)
          ~doc:
            " Override the number of window bits used in the algorithm, to simulate a \
             smaller number of buckets"
      and seed =
        flag
          "-seed"
          (optional_with_default 0 int)
          ~doc:" The seed to use for point generation"
      and set_scalars_to_one =
        flag "-set-scalars-to-one" no_arg ~doc:"Force the scalars to always be 1"
      and set_all_points_to_trivial =
        flag
          "-set-all-points-to-trivial"
          no_arg
          ~doc:"Force all the affine points to always be identity"
      in
      fun () ->
        let module Config = struct
          include Msm_pippenger.Config.Bls12_377

          let scalar_bits = Option.value scalar_bits_arg ~default:scalar_bits
          let window_size_bits = Option.value window_bits_arg ~default:window_size_bits
        end
        in
        let module Utils = Msm_pippenger_test_top.Utils.Make (Config) in
        let module Top = Msm_pippenger.Top.Make (Config) in
        let module Test_kernel = Msm_pippenger_test_top.Test_kernel_for_vitis.Make (Config)
        in
        let params =
          Lazy.force Twisted_edwards_model_lib.Bls12_377_params.twisted_edwards
        in
        let q = Ark_bls12_377_g1.modulus () in
        printf "q = 0x%s\n%!" (Z.format "x" q);
        printf "a = 0x%s\n%!" (Z.format "x" params.a);
        printf "d = 0x%s\n%!" (Z.format "x" params.d);
        printf "twisted_scale = 0x%s\n%!" (Z.format "x" params.twisted_scale);
        let input_points = Utils.random_inputs ~seed num_points in
        let input_points =
          if set_scalars_to_one
          then
            Array.map input_points ~f:(fun p ->
              { p with scalar = Bits.one (Bits.width p.scalar) })
          else input_points
        in
        let input_points =
          if set_all_points_to_trivial
          then
            Array.mapi input_points ~f:(fun i p ->
              let x = Bits.of_int ~width:377 i in
              let y = Bits.one 377 in
              let t = x in
              { p with affine_point_with_t = { x; y; t }; affine_point = { x; y } })
          else input_points
        in
        Out_channel.write_all
          input_filename
          ~data:
            (Array.map input_points ~f:(fun data ->
               let bits =
                 Bits.(
                   data.scalar
                   @: Utils.Affine_point_with_t.Of_bits.pack data.affine_point_with_t)
               in
               Bits.uresize bits (512 * Test_kernel.num_clocks_per_input)
               |> Bits.to_constant
               |> Constant.to_hex_string ~signedness:Unsigned)
            |> Array.to_list
            |> String.concat ~sep:"\n");
        (* Do the bucket sums like the FPGA will. *)
        let windows =
          Array.init Top.num_windows ~f:(fun window ->
            Array.init
              (if window = Top.num_windows - 1
              then 1 lsl Top.last_window_size_bits
              else 1 lsl Config.window_size_bits)
              ~f:(fun _ ->
                Utils.Twisted_edwards.(affine_identity |> affine_to_extended ~z:Z.one)))
        in
        let to_z b = Bits.to_constant b |> Constant.to_z ~signedness:Unsigned in
        Array.iter input_points ~f:(fun input ->
          let p : Utils.Twisted_edwards.affine_with_t =
            { x = to_z input.affine_point_with_t.x
            ; y = to_z input.affine_point_with_t.y
            ; t = to_z input.affine_point_with_t.t
            }
          in
          for i = 0 to Top.num_windows - 1 do
            let upper_bound =
              if i = Top.num_windows - 1
              then (i * Config.window_size_bits) + Top.last_window_size_bits
              else (i + 1) * Config.window_size_bits
            in
            let slice =
              Bits.(select input.scalar (upper_bound - 1) (i * Config.window_size_bits))
            in
            let bucket = Bits.to_int slice in
            windows.(i).(bucket)
              <- Utils.Twisted_edwards.add_unified
                   (force Twisted_edwards_model_lib.Bls12_377_params.twisted_edwards)
                   windows.(i).(bucket)
                   p
          done);
        let of_z z = Constant.of_z ~width:Config.field_bits z |> Bits.of_constant in
        Out_channel.write_all
          output_filename
          ~data:
            (Array.map windows ~f:(fun bucket ->
               (* Don't do the 0th bucket. *)
               List.init
                 (Array.length bucket - 1)
                 ~f:(fun i ->
                   let v = bucket.(Array.length bucket - 1 - i) in
                   let b =
                     { Utils.Extended.t = of_z v.t
                     ; x = of_z v.x
                     ; y = of_z v.y
                     ; z = of_z v.z
                     }
                   in
                   let b_packed =
                     Bits.uresize
                       (Utils.Extended.Of_bits.pack b)
                       (Test_kernel.num_clocks_per_output * 512)
                   in
                   Constant.to_hex_string ~signedness:Unsigned (Bits.to_constant b_packed))
               |> String.concat ~sep:"\n")
            |> Array.to_list
            |> String.concat ~sep:"\n")]
;;

let () =
  Command_unix.run
    (Command.group ~summary:"MSM tools" [ "test-vectors", command_test_vectors ])
;;
