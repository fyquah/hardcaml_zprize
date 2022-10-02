open Test_top

let%expect_test "Test over more inputs" =
  let module Config = struct
    let field_bits = 377
    let scalar_bits = 12
    let controller_log_stall_fifo_depth = 2
    let num_windows = 4
    let window_ram_partition_settings = None
  end
  in
  let module Test = Make (Config) in
  let _result = Test.run_test 64 in
  [%expect
    {|
    (Expecting (Top.num_result_points 19))
    (Got ("List.length (!result_points)" 19))
    PASS |}]
;;
