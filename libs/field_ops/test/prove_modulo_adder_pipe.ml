let%expect_test "" =
  for num_inputs = 2 to 3 do
    for stages = 1 to 4 do
      let result =
        match
          Prove_modulo_adder_subtractor_pipe.test ~op:`Add ~bits:32 ~stages ~num_inputs
        with
        | Ok () -> "OKAY!"
        | Error _ -> "FAILED!"
      in
      Stdio.printf "num_inputs=%d, stages=%d: %s\n" num_inputs stages result
    done
  done;
  [%expect
    {|
    num_inputs=2, stages=1: OKAY!
    num_inputs=2, stages=2: OKAY!
    num_inputs=2, stages=3: OKAY!
    num_inputs=2, stages=4: OKAY!
    num_inputs=3, stages=1: OKAY!
    num_inputs=3, stages=2: OKAY!
    num_inputs=3, stages=3: OKAY!
    num_inputs=3, stages=4: OKAY! |}]
;;
