open Core
open Hardcaml

let command_test_vectors =
  Command.basic
    ~summary:
      "Generate input and output points that can be read by AWS host test harnesses"
    [%map_open.Command
      let num_points =
        flag "-num-points" (required int) ~doc:"The number of points to simulate over"
      and input_filename =
        flag "-input-filename" (required string) ~doc:"File to write input points to"
      in
      fun () ->
        let module Utils = Msm_pippenger_test.Utils.Make (Msm_pippenger.Config.Bls12_377)
        in
        let num_points = Utils.random_inputs num_points in
        Out_channel.write_all
          input_filename
          ~data:
            (Array.map num_points ~f:(fun data ->
               Bits.(
                 data.scalar
                 @: Utils.Affine_point_with_t.Of_bits.pack data.affine_point_with_t)
               |> Bits.to_constant
               |> Constant.to_hex_string ~signedness:Unsigned)
            |> Array.to_list
            |> String.concat ~sep:"\n")]
;;

let () =
  Command_unix.run
    (Command.group ~summary:"MSM tools" [ "test-vectors", command_test_vectors ])
;;
