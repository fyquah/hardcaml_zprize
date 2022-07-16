open Core
open Setup_stream

let%expect_test "Test msm_setup_precomputed_points_stream" =
  let base_x = Z.of_string "0x123456789ab000" in
  let base_y = Z.of_string "0x14239847239429" in
  let num_points = 10 in
  let x = Bytes.init (num_points * 48) ~f:(fun _ -> '\000') in
  let y = Bytes.init (num_points * 48) ~f:(fun _ -> '\000') in
  let dst_num_bytes_expected = 32 + (num_points * 2 * 48) in
  let dst = Bytes.create dst_num_bytes_expected in
  for i = 0 to num_points - 1 do
    let f = Bytes.unsafe_of_string_promise_no_mutation in
    Bytes.blito ~dst:x ~dst_pos:(i * 48) ~src:(f Z.(to_bits (add base_x (of_int i)))) ();
    Bytes.blito ~dst:y ~dst_pos:(i * 48) ~src:(f Z.(to_bits (add base_y (of_int i)))) ()
  done;
  let address = 0xdeadbeef in
  let num_bytes_written =
    msm_setup_precomputed_points_stream ~address ~num_points ~x ~y ~dst
  in
  Stdio.printf "Number of bytes written: %d\n" num_bytes_written;
  Stdio.printf "Number of bytes expected: %d\n" dst_num_bytes_expected;
  [%expect {|
    Number of bytes written: 992
    Number of bytes expected: 992 |}];
  assert (num_bytes_written = dst_num_bytes_expected);
  assert (Int64.equal (Bytes.unsafe_get_int64 dst 0) (Int64.of_int address));
  for i = 0 to num_points - 1 do
    let point_x =
      Bytes.unsafe_to_string
        ~no_mutation_while_string_reachable:(Bytes.subo dst ~pos:(32 + (i * 96)) ~len:48)
      |> Z.of_bits
    in
    let point_y =
      Bytes.unsafe_to_string
        ~no_mutation_while_string_reachable:
          (Bytes.subo dst ~pos:(32 + 48 + (i * 96)) ~len:48)
      |> Z.of_bits
    in
    assert (Z.(equal (add base_x (of_int i)) point_x));
    assert (Z.(equal (add base_y (of_int i)) point_y))
  done
;;

let get_uint16 b pos =
  let b0 = Char.to_int (Bytes.get b pos) in
  let b1 = Char.to_int (Bytes.get b (pos + 1)) in
  (b1 lsl 8) lor b0
;;

let%expect_test "Test msm_setup_scalars_stream" =
  let num_scalars = 10 in
  let num_scalars_per_batch = 4 in
  let num_bits = 4 in
  let scalars = List.init num_scalars ~f:Z.of_int in
  let dst = Bytes.create 1_000 in
  let num_bytes_written =
    msm_setup_scalars_stream
      ~num_scalars
      ~num_scalars_per_batch
      ~num_bits
      ~scalars_buffer:(Scalar_buffer.of_scalars scalars)
      ~dst
  in
  Stdio.printf "Number of bytes written to dst = %d\n" num_bytes_written;
  [%expect {| Number of bytes written to dst = 24 |}];
  List.iteri scalars ~f:(fun i scalar ->
      if i % 4 = 0 then Stdio.print_endline "";
      Stdio.printf "%s\n" (Z.format "04b" scalar));
  [%expect
    {|
    0000
    0001
    0010
    0011

    0100
    0101
    0110
    0111

    1000
    1001 |}];
  assert (num_bytes_written % 2 = 0);
  let num_addresses = num_bytes_written / 2 in
  for i = 0 to num_addresses - 1 do
    if i % 3 = 0 then Stdio.printf "\n";
    let address = get_uint16 dst (2 * i) in
    Stdio.printf "%s\n" (Z.format "06b" (Z.of_int address))
  done;
  [%expect
    {|
    000000
    010000
    100011

    000000
    011111
    100000

    001100
    011100
    100000

    001010
    011010
    100010 |}]
;;
