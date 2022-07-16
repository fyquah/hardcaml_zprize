open Core
open Hardcaml_waveterm
open Test_top

let%expect_test "Test double faults" =
  let waves, sim = Waveform.create (create_sim ()) in
  let dump_waveform () = Waveform.Serialize.marshall waves "b.hardcamlwaveform.Z" in
  let batch_size = 2 in
  let scalar_num_bits = 3 in
  let scalars = [ 4; 2; 2; 2; 2; 1; 2; 1; 2; 1 ] in
  let field_points =
    let generator = Ark_bls12_377_g1.subgroup_generator () in
    [ Ark_bls12_377_g1.mul ~by:1 generator
    ; Ark_bls12_377_g1.mul ~by:2 generator
    ; Ark_bls12_377_g1.mul ~by:42 generator
    ; Ark_bls12_377_g1.mul ~by:24 generator
    ; Ark_bls12_377_g1.mul ~by:1 generator
    ; Ark_bls12_377_g1.mul ~by:2 generator
    ; Ark_bls12_377_g1.mul ~by:1 generator
    ; Ark_bls12_377_g1.mul ~by:2 generator
    ; Ark_bls12_377_g1.mul ~by:1 generator
    ; Ark_bls12_377_g1.mul ~by:2 generator
    ]
  in
  Core.protect
    ~f:(fun () ->
      test ~wiggle_tvalid:false ~batch_size ~scalar_num_bits ~scalars ~field_points sim)
    ~finally:dump_waveform;
  [%expect
    {|
    (scalars (0x4 0x2 0x2 0x2 0x2 0x1 0x2 0x1 0x2 0x1))
    num scalars/field points: 10
    batch size: 2
    num tasks: 5
    Number of bytes in scalar stream: 30
    Expected on curve?: true
    Obtained on curve?: true
    Expected matches obtained?: true
    ((expected
      ((x
        0xbe4fc3b0763038c3c3b2b050752f93fead045a6425f03048cf0237c4b85931f96b9484090aae2cd7f75ad556c65c4a)
       (y
        0x77f7945f9743f61a0021c2a9e06196ef6cf30ff7aa31f952d4fb8e861dd41a66f77a134dbcd526e7aa8e97cf997141)
       (infinity false)))
     (obtained
      ((x
        0xbe4fc3b0763038c3c3b2b050752f93fead045a6425f03048cf0237c4b85931f96b9484090aae2cd7f75ad556c65c4a)
       (y
        0x77f7945f9743f61a0021c2a9e06196ef6cf30ff7aa31f952d4fb8e861dd41a66f77a134dbcd526e7aa8e97cf997141)
       (infinity false)))) |}]
;;
