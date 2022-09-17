open Test_top

let%expect_test "Test over more inputs" =
  let module Config = struct
    let field_bits = 377
    let scalar_bits = 9
    let controller_log_stall_fifo_depth = 2
    let window_size_bits = 3
    let ram_read_latency = 1
  end
  in
  let module Test = Make (Config) in
  let _result = Test.run_test 64 in
  [%expect {|
    (Expecting (Top.num_result_points 21))
    (Got ("List.length (!result_points)" 21))
    PASS |}]
;;
