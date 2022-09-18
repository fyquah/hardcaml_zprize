open Test_top
open Msm_pippenger_multi_slr

let%expect_test "Test over more inputs" =
  let module Config = struct
    let t =
      { Pippenger_compute_unit.Pippenger_compute_unit_config.field_bits = 377
      ; scalar_bits = 9
      ; controller_log_stall_fifo_depth = 2
      ; window_size_bits = 3
      ; ram_read_latency = 1
      }
    ;;
  end
  in
  let module Test = Make (Config) in
  let _result = Test.run_test 64 in
  [%expect
    {|
    (Expecting (Top.num_result_points 21))
    (Got ("List.length (!result_points)" 21))
    PASS |}]
;;
