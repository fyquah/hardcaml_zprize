open Test_top

let%expect_test "Test over more inputs" =
  let module Config = struct
    let field_bits = 377
    let scalar_bits = 12
    let controller_log_stall_fifo_depth = 2
    let window_size_bits = 3
  end
  in
  let module Test = Make (Config) in
  let _result = Test.run_test 64 in
  [%expect
    {|
    "ERROR: Timeout during simulation!"
    "ERROR: Timeout during simulation!"
    (Expecting (Top.num_result_points 28))
    (Got ("List.length (!result_points)" 0))
    "ERROR: Timeout during simulation!"
    ("ERROR: Result points did not match!"
     (expected
      ((x
        0x1887f4a367ca8900f078692fd15034191996736965c1473a657f5918ef42a6f38465d115ecccf3b60deeff14d468220)
       (y
        0xba23a800d0a46a8b012acbd400aaf872aabac9b86affd41abc86fc7b34f905a96c42f3ebbcdee663082048e9616cfb)
       (infinity false)))
     (fpga_calculated_result ((x 0x0) (y 0x1) (infinity true)))) |}]
;;
