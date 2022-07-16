open Core
open Hardcaml_waveterm
open Test_top

let%expect_test "Wiggle tvalid and tready in top level" =
  let waves, sim = Waveform.create (create_sim ()) in
  let dump_waveform () = Waveform.Serialize.marshall waves "e.hardcamlwaveform.Z" in
  let batch_size = 1 in
  let scalar_num_bits = 1 in
  let num_scalars = 100 in
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
      test
        ~sleep_for_cycles_until_accepting_result:100
        ~wiggle_tvalid:true
        ~batch_size
        ~scalar_num_bits
        ~scalars
        ~field_points
        sim)
    ~finally:dump_waveform;
  [%expect
    {|
    (scalars
     (0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1
      0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1
      0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1
      0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1
      0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1 0x1
      0x1 0x1 0x1 0x1 0x1))
    num scalars/field points: 100
    batch size: 1
    num tasks: 100
    Number of bytes in scalar stream: 200
    Expected on curve?: true
    Obtained on curve?: true
    Expected matches obtained?: true
    ((expected
      ((x
        0x7f34eeb9dcf0c8eec72f430911cc90cb4d84f626c80a660e92f90ca3d36e9f65c98abda434647004b78195ee5d839)
       (y
        0x1c72a80762e3c28f08e7454a8b2e74a020fe42e248d29282e4f20505f7ec4ecd6bb4c41e1c819cfdfc148fc6ae6e65)
       (infinity false)))
     (obtained
      ((x
        0x7f34eeb9dcf0c8eec72f430911cc90cb4d84f626c80a660e92f90ca3d36e9f65c98abda434647004b78195ee5d839)
       (y
        0x1c72a80762e3c28f08e7454a8b2e74a020fe42e248d29282e4f20505f7ec4ecd6bb4c41e1c819cfdfc148fc6ae6e65)
       (infinity false)))) |}]
;;
