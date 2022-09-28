open Test_top

let%expect_test "Test over more inputs" =
  let module Config = struct
    let field_bits = 377
    let scalar_bits = 18
    let controller_log_stall_fifo_depth = 2
    let window_size_bits = 3
  end
  in
  let module Test = Make (Config) in
  let _result = Test.run_test 64 in
  [%expect
    {|
    (Expecting (Top.num_result_points 28))
    (Got ("List.length (!result_points)" 28))
    PASS |}]
;;
