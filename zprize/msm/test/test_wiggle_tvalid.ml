open Core
open Hardcaml_waveterm
open Test_top

let%expect_test "Wiggle tvalid and tready in top level" =
  let waves, sim = Waveform.create (create_sim ()) in
  let dump_waveform () = Waveform.Serialize.marshall waves "c.hardcamlwaveform.Z" in
  let batch_size = 1 in
  let scalar_num_bits = 3 in
  let num_scalars = 20 in
  let scalars =
    List.init num_scalars ~f:(fun _ -> Random.int_incl 1 ((1 lsl scalar_num_bits) - 1))
  in
  let field_points =
    let generator = Ark_bls12_377_g1.subgroup_generator () in
    List.init num_scalars ~f:(fun _ ->
        let by = Random.int_incl 1 ((1 lsl scalar_num_bits) - 1) in
        Ark_bls12_377_g1.mul ~by generator)
  in
  Core.protect
    ~f:(fun () ->
      test ~wiggle_tvalid:true ~batch_size ~scalar_num_bits ~scalars ~field_points sim)
    ~finally:dump_waveform;
  [%expect
    {|
    (scalars
     (0x5 0x3 0x2 0x7 0x4 0x3 0x3 0x2 0x2 0x3 0x1 0x2 0x7 0x1 0x2 0x6 0x7 0x7 0x4
      0x6))
    num scalars/field points: 20
    batch size: 1
    num tasks: 20
    Number of bytes in scalar stream: 120
    Expected on curve?: true
    Obtained on curve?: true
    Expected matches obtained?: true
    ((expected
      ((x
        0xbd42de4af6287c22aa37d7c264e4857fa3c9595a65076b4dd7c761b4bc97d77e36fefae163c4951a187c4adc232cfd)
       (y
        0x15ebbd30c82adad1696ebc1c6f9f769730d93a4b45aedf76033ae56a38e89bea5efe375d750ba1e8258b766b46e3194)
       (infinity false)))
     (obtained
      ((x
        0xbd42de4af6287c22aa37d7c264e4857fa3c9595a65076b4dd7c761b4bc97d77e36fefae163c4951a187c4adc232cfd)
       (y
        0x15ebbd30c82adad1696ebc1c6f9f769730d93a4b45aedf76033ae56a38e89bea5efe375d750ba1e8258b766b46e3194)
       (infinity false)))) |}]
;;
