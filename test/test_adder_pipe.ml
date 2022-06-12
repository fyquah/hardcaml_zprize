let%expect_test "" =
  for num_inputs = 2 to 5 do
    for stages = 1 to 5 do
      let result =
        match Test_adder_subtractor_pipe.test ~op:`Add ~bits:32 ~stages:8 ~num_inputs:3 with
        | Ok () -> "OKAY!"
        | Error _ -> "FAILED!"
      in
      Stdio.printf  "num_inputs=%d, stages=%d: %s\n" num_inputs stages result
    done;
  done;
  [%expect {|
    num_inputs=2, stages=1: OKAY!
    num_inputs=2, stages=2: OKAY!
    num_inputs=2, stages=3: OKAY!
    num_inputs=2, stages=4: OKAY!
    num_inputs=2, stages=5: OKAY!
    num_inputs=3, stages=1: OKAY!
    num_inputs=3, stages=2: OKAY!
    num_inputs=3, stages=3: OKAY!
    num_inputs=3, stages=4: OKAY!
    num_inputs=3, stages=5: OKAY!
    num_inputs=4, stages=1: OKAY!
    num_inputs=4, stages=2: OKAY!
    num_inputs=4, stages=3: OKAY!
    num_inputs=4, stages=4: OKAY!
    num_inputs=4, stages=5: OKAY!
    num_inputs=5, stages=1: OKAY!
    num_inputs=5, stages=2: OKAY!
    num_inputs=5, stages=3: OKAY!
    num_inputs=5, stages=4: OKAY!
    num_inputs=5, stages=5: OKAY! |}]
;;
