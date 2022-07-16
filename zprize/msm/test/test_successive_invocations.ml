open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Test_top

let%expect_test "Test successive invocations" =
  let waves, sim = Waveform.create (create_sim ()) in
  let dump_waveform () = Waveform.Serialize.marshall waves "f.hardcamlwaveform.Z" in
  let batch_size = 1 in
  let scalar_num_bits = 1 in
  let num_scalars = 20 in
  let test_one_iter ~clear () =
    let scalars =
      List.init num_scalars ~f:(fun _ -> Random.int_incl 1 ((1 lsl scalar_num_bits) - 1))
    in
    let field_points =
      let generator = Ark_bls12_377_g1.subgroup_generator () in
      List.init num_scalars ~f:(fun _ ->
          let by = Random.int_incl 1 Int.max_value in
          Ark_bls12_377_g1.mul ~by generator)
    in
    test
      ~clear
      ~wiggle_tvalid:false
      ~batch_size
      ~scalar_num_bits
      ~scalars
      ~field_points
      sim
  in
  protect
    ~f:(fun () ->
      test_one_iter ~clear:true ();
      [%expect
        {|
        (scalars
         (0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1
          0x1))
        num scalars/field points: 20
        batch size: 1
        num tasks: 20
        Number of bytes in scalar stream: 40
        Expected on curve?: true
        Obtained on curve?: true
        Expected matches obtained?: true
        ((expected
          ((x
            0x25d35e96bce759ebf5a1eee9b2d7309e7c69f7c0c372bcc2121f0c0a7924113784cdadfbd1582e45669e5bb3e13707)
           (y
            0x518d4e187e6ba558633dac84a9b5c0fc4d4c4d8926b6249bbfa6c6377eadaa8f708844593a7afcd5680f10d7569e3b)
           (infinity false)))
         (obtained
          ((x
            0x25d35e96bce759ebf5a1eee9b2d7309e7c69f7c0c372bcc2121f0c0a7924113784cdadfbd1582e45669e5bb3e13707)
           (y
            0x518d4e187e6ba558633dac84a9b5c0fc4d4c4d8926b6249bbfa6c6377eadaa8f708844593a7afcd5680f10d7569e3b)
           (infinity false)))) |}];
      test_one_iter ~clear:false ();
      [%expect
        {|
        (scalars
         (0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1
          0x1))
        num scalars/field points: 20
        batch size: 1
        num tasks: 20
        Number of bytes in scalar stream: 40
        Expected on curve?: true
        Obtained on curve?: true
        Expected matches obtained?: true
        ((expected
          ((x
            0x168f7e289e50290300168b7f79d6dee0c3cd4d0af4521b9c1a32eea5a3f551e3101d28feb54cac829f30109186ccb0)
           (y
            0x16393ca0cfd90a1bb0460d11f89955aec37381513fc79aa3869acfe84b9cd8cc8c02b2c17ddcc78011e42f2750ef986)
           (infinity false)))
         (obtained
          ((x
            0x168f7e289e50290300168b7f79d6dee0c3cd4d0af4521b9c1a32eea5a3f551e3101d28feb54cac829f30109186ccb0)
           (y
            0x16393ca0cfd90a1bb0460d11f89955aec37381513fc79aa3869acfe84b9cd8cc8c02b2c17ddcc78011e42f2750ef986)
           (infinity false)))) |}])
    ~finally:dump_waveform
;;
