open Core
open Hardcaml
open Hardcaml_waveterm
module N4 = Ntts_r_fun.Ntt_4step
module Gf_z = Ntts_r_fun.Gf_z
module Gf_bits = Ntts_r_fun.Gf_bits.Make (Bits)
module Kernel = Ntts_r_fun.Ntt_4step.Kernel
module Sim = Cyclesim.With_interface (Kernel.I) (Kernel.O)

let logn = N4.logn
let n = 1 lsl logn
let logcores = N4.logcores
let num_cores = 1 lsl logcores
let log_passes = logn - logcores
let num_passes = 1 lsl log_passes

let input_coefs () =
  Array.init num_passes ~f:(fun _ ->
      Array.init num_cores ~f:(fun _ ->
          Array.init n ~f:(fun _ -> Z.of_int (Random.int 100_000))))
;;

let print_matrices c =
  for pass = 0 to num_passes - 1 do
    for i = 0 to n - 1 do
      printf "%.2i:" i;
      for core = 0 to num_cores - 1 do
        let c =
          c.(pass).(core).(i)
          |> Gf_z.to_z
          |> Gf_bits.of_z
          |> Gf_bits.to_bits
          |> Bits.to_constant
          |> Constant.to_hex_string ~signedness:Unsigned
        in
        printf "%s " c
      done;
      printf "\n"
    done;
    printf "\n"
  done
;;

let reference ~verbose input_coefs =
  let module Ntt = Ntts_r_fun.Ntt_sw.Make (Gf_z) in
  let c = Array.map input_coefs ~f:(Array.map ~f:(Array.map ~f:Gf_z.of_z)) in
  if verbose
  then (
    printf "\n\nINPUTS\n\n";
    print_matrices c);
  Array.iter c ~f:(Array.iter ~f:(fun c -> Ntt.inverse_dit c));
  if verbose
  then (
    printf "\n\nREFERENCE\n\n";
    print_matrices c);
  c
;;

let run_simple () =
  let sim =
    Sim.create
      ~config:Cyclesim.Config.trace_all
      (Kernel.create
         ~build_mode:Simulation
         (Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ()))
  in
  let inputs = Cyclesim.inputs sim in
  let waves, sim = Waveform.create sim in
  inputs.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clear := Bits.gnd;
  inputs.start := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.start := Bits.gnd;
  Cyclesim.cycle sim;
  inputs.data_in.tvalid := Bits.vdd;
  inputs.data_out_dest.tready := Bits.vdd;
  for _ = 0 to 300 do
    Cyclesim.cycle sim
  done;
  waves
;;

let get_results (results : Bits.t list) =
  let a =
    List.map (List.rev results) ~f:(fun b ->
        List.map (Bits.split_lsb ~part_width:64 b) ~f:(fun x ->
            Bits.to_z ~signedness:Unsigned x |> Gf_z.of_z)
        |> Array.of_list)
    |> List.groupi ~break:(fun i _ _ -> i % n = 0)
    |> List.map ~f:Array.of_list
    |> Array.of_list
  in
  assert (Array.length a = num_passes);
  Array.init num_passes ~f:(fun pass ->
      Array.init num_cores ~f:(fun core -> Array.init n ~f:(fun n -> a.(pass).(n).(core))))
;;

let run ?(verbose = false) () =
  Random.init 100;
  let sim =
    Sim.create
      ~config:Cyclesim.Config.trace_all
      (Kernel.create
         ~build_mode:Simulation
         (Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ()))
  in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let waves, sim = Waveform.create sim in
  let input_coefs = input_coefs () in
  let results = ref [] in
  let cycle () =
    if Bits.to_bool !(outputs.data_out.tvalid)
    then results := !(outputs.data_out.tdata) :: !results;
    Cyclesim.cycle sim
  in
  inputs.clear := Bits.vdd;
  cycle ();
  inputs.clear := Bits.gnd;
  inputs.data_out_dest.tready := Bits.vdd;
  inputs.start := Bits.vdd;
  cycle ();
  inputs.start := Bits.gnd;
  cycle ();
  for pass = 0 to num_passes - 1 do
    (* wait for tready *)
    while not (Bits.to_bool !(outputs.data_in_dest.tready)) do
      cycle ()
    done;
    for i = 0 to n - 1 do
      inputs.data_in.tvalid := Bits.vdd;
      inputs.data_in.tdata
        := List.init num_cores ~f:(fun core -> input_coefs.(pass).(core).(i))
           |> List.map ~f:(fun z -> Gf_bits.of_z z |> Gf_bits.to_bits)
           |> Bits.concat_lsb;
      cycle ()
    done;
    inputs.data_in.tvalid := Bits.gnd;
    (* wait for tready to go low. *)
    while Bits.to_bool !(outputs.data_in_dest.tready) do
      cycle ()
    done;
    for _ = 0 to 3 do
      cycle ()
    done
  done;
  for _ = 0 to 200 do
    cycle ()
  done;
  let results = get_results !results in
  print_matrices results;
  let reference = reference ~verbose input_coefs in
  if [%equal: Gf_z.t array array array] results reference
  then printf "IT WORKED!!!\n"
  else printf "ERROR!!!\n";
  waves
;;

let%expect_test "" =
  let waves = run () in
  Waveform.print
    ~start_cycle:60
    ~display_width:94
    ~display_height:76
    ~wave_width:(-1)
    waves;
  [%expect
    {|
    00:00000000001c63ca 000000000017a5ed 00000000001964da 00000000001cea35 000000000015003f 00000000001a2da7 000000000017c597 000000000018f849
    01:bb5174a8f03db251 c92971fa9b0b7b6e d353b5953cb1f3de 0ba0a63c4957680d bbb707f3fef8e342 d3da5d232f854da9 1487648057e4b4dc 4d1cee5f7becfaba
    02:dd1e41a18d1c3037 34d6bc6a5210029c 29e58b54fce540f3 a0c964c882cb3fb1 f5e3430ef01a4a9a 98bef79791e5b432 af31724909117a13 0523c6cf5969a832
    03:9f5d00052fc000ed e0deb29db578f3f6 bba445e89b853fee 3f8e9ffb17ead0cf d24cffce876d0c7e a47dfdf5c9c5ca65 09054fd1eab3a6d1 d9790bb3526d3aad
    04:e55a42a7d72a0a61 573222d0cfa127fb 4d208fc8bb2ceb52 bdba99bd2a150827 b286496bfc60efce 0670a6f2aed1db2f a933401a82084aa0 309ca37ce0b9fbcc
    05:fdb34d2761187ae8 e944e4009fecb2d6 0b1dfd226e9e3c9d e5937e8e52cb409e f843bb87ee20724f b8b8f84fa0c94f9e d9615764d5801951 cf055103bd94ac76
    06:a5777853ad5c984c 2b47b6158fee4776 be0f972306f33cf3 2eeaf723acbdb7ed df0755d7e2914508 86d03b7613de45f4 dd7e6a689eafe4ec 1114efe257e01027
    07:b174e232949df8fd 0bf2119b509e4c11 8767b5335651a78d c6bf528a7fbeff57 4959886a94f9be67 b51f2b0d29641df3 586963ab24f0d5ef 0ad4b984ab619d16
    08:7c8f000000007d45 3e24fffefffffd6d 303d000000005fef 8ffbffff0002e7ac 9ca90001000060cb d53dffff0002641c 58d700020000896c e13cfffffffed854
    09:f75e3385a3326e47 c0f41d5fb985b285 a3f5a982e89ed2c6 9a98eda4d3bad7d1 ec790d45dfe38393 7299276d7f28f64b 89a80aa71ec87d10 943cfa3a42a35310
    10:1c8c5c5b1bb62950 7b6259a77b647186 36c95dbb9b4a909b 3ca9e776785856e6 dfaf649daf8091bb 35937f861b9d013a 043256f6e2e82c8d 366a08dbc6e45bfd
    11:e6275b9149a779b1 16b82fb2c80d9395 5ac0f5c0e82cb566 075ea34d147e8d97 c6f1704d4502a054 1868598ebfbae9d7 5b7ac677e1d96d8f 82333fc684f73aee
    12:1921d2d341587764 a6fdce5e252d7cfa b679c2d09035bc53 45ef2deb96427426 4b85fb9f4c913ecb f7b2af2fab0bd52c 55de7ff63fe3099f d126e345a285bccf
    13:75cfcc11ccc7c55a 461b4120109827fe 147899bb2b9dfc39 170569a1c3012063 f5cab77e069a7458 8f42ba1d4aeead1e 467c4c51350636a0 718549956ee45376
    14:a3a46e8b85f6d2a2 81ba7b2330bc4da2 504f100da38dd45c 0f61c0a07b79abfe 4e5374bd22152b5e 62994b8cd277d658 9db0c7fdfe460d22 9a1a54a8a0d2d9dc
    15:7ee5e30cea310cbf 40668ce81fccb69a 2a7740f4b0a848a1 1bff5cce80b3df08 1a4b6f8effa3cf91 440606134fa5fab7 8f1910c2aad56c63 dbc4efc31e3f96ea
    16:000000000000b7f4 000000000000c0f3 0000000000029760 fffffffefffbaac6 0000000000021a21 0000000000027237 fffffffefffd692a 000000000004487b
    17:176b072731cd0ce0 4f8bd3a6b6fbf2fa f474d3b26626e623 c9dd2c09f6e2eab0 e9f63416dd112209 0030bd6bb2dd9d5b f4907e2636ef3c02 a85aecfccb252101
    18:a937d076aa4f1151 52eda749be88898e 77681ab34065ff45 a35c432dc6a8d391 b786c99d16b0d469 3d5cf91d58a7029f 4fd3e00407d997e2 c131831706101370
    19:1bb4646f2b46f86b bbf742277ef3980d 1fbb8d1131f4b471 638d0408cff560c1 770b0db51a54d0b0 25c5e11f6219e43d be43ce1b3b65283a 99f626b0751c125e
    20:e9afbd5a28d48e61 5875dd2d305d6ffb 457d703744d00b52 b9e36640d5ec3827 b349b690039d85ce 06135909512f2d2f ab46bfe37df4caa0 2e0d5c851f4941cc
    21:976528d1b842d124 b2d28c3a60ceced4 ac0d2fa803463e16 c5867fce9c2aa79b cf68554df975b6e5 213e5cbfa0e9ac5a e258037f2221909a daa5240a4a5d44b1
    22:d0e0bc3f688c873f 4c125d53d85c98b1 9f60a790a3056d47 89714f65ff4bbd57 716faf5a2cc0e1fc a13ba9fadb6e78e1 22dda8a9b7046f0d 28fd709df2389580
    23:9a1c0b97e721ce2d 67e61999a977097d 655dff209d77aab3 0198949ae6a38555 9d28d4ef55003359 339f9e0eee468abf 80f210b2c067a958 d54b2fa309c5b7a1
    24:8370ffff00007d46 c1dafffffffffd6c cfc2ffff00005ff0 700400000002e7ab 6356fffe000060ce 2ac200000002641b a728fffd00008971 1ec2fffefffed855
    25:d81ae15f88a04414 d5ae0968cfa35d32 8ce10b725065d183 ce3841ea46ef2fc6 19975ec4f0e42bae a241bd19b90580a5 c5eec1e2fe05b16c 244c3ef7ffbe8d43
    26:6c9d918dace14037 099142a07404e6a4 6918fc3c276bff9d 0a6470943e33105b cf528eb449b2a948 a5a08fc6f9d79597 f8a856be0c2d8d68 8ee4ad41d99bf3a5
    27:bf70921eea933e78 9e52b051671cee8e d39e136184173486 122b0dcb09cf4730 433c2557b925330f 38f1ecc7154ea97e 8822cffba8d7e2b8 5b7a1460c6f00015
    28:17d42d28bea62164 a95a31a1dad11afa b6e83d2d6fc73a53 4272d21469becc26 4eaa0462b36d36cb fbc950d254f5332c 55a78009c01a0b9f d02f1cb65d7d80cf
    29:930a2d2fcbfd8026 d385e23d137ba477 2cbcfb3b86997f5c 81f1962bf32b59b8 f7eb8f9564fc5f01 74cff1bd58cbb8de 4dbba98e27b79ecf 95b72cc8ffb7df72
    30:d6835cdb6422b8e9 fa33717466fab686 1110b13bb27b51dd ad0df8d1d87c5946 04c9860dce9707a5 c40acefc3e3cb87d 661324e9ac066ad7 a02f4ad0150e8bcc
    31:94b7dd0a0acb7d98 94d0730e8286b1fd ee042e9521c9fb6a dca366e912c252a4 4a8c8fe576773f3f f0ad0b5e97c4de8f 4404c682bf0993a8 9416a086192aac6d

    00:000000000016b317 000000000018d59e 000000000017d405 00000000001bca74 000000000014538d 00000000001b777b 000000000015b76a 0000000000165343
    01:12b89f3c75d80ece bb01d7e0915ea7bb aa08da502797425f 33d75db0e19ce84b db942390e6c84d03 868201942b358b53 5c76504e2d7d5af9 17d28a39ad607cc3
    02:ff2d397a10303258 459b8140b13abdd1 788bf00e2c2e2f2a 1fe13b9d6dd4a997 f9fb0a8e991e9050 15fae4e583b7fb72 08d652cd3bd4ec2b 283bc1c4c1c580c2
    03:7596d9889c4b1b8b 99b4c35b32fa9d7e 4c4de56ef29d50bb 15a6471e237134dc 9b161f8d57a22b35 228c84d6e9c8a3f6 371548b2ad7c482d 296a3941fdb9a658
    04:7483e174a9027137 f83428fa2c9402ce 8efcdf9d4d7d62e3 36bc7242dff40b0f 7d31796daa0789eb cb6129a458636cd9 b327dc9fb5e699ea 394465d006103cab
    05:67fdf58486aed8a2 64c6c652d9e7da4f 77d908ed0a4e61e1 447d09790e691f0c bff47fba8505d098 52e6aa7d84c0389d bc848d5dfdc43114 1fc887b3a8f3a35a
    06:d599fd132f05d797 f893f7601290267e 4ebac00a9116e16b 1f54fa5b759c2430 18ecfbc2ff804bd6 597849877e4dad16 e2a3c4923d452e9c 0ffd9e276d45d55a
    07:ef9a3076b935f861 95661fdfea3f7ac8 f19b300899bfb0b9 cad49f75ff1104f2 11bdfbe3cae5a726 90059b3a60f29a4d bf145431fed3273c d8b01aafc7fc2e59
    08:a7ddfffcfffde480 3a5c00000001814e 8911fffffffc63c5 366dfffefffe27f7 d4090000000092b0 7fd000010000d038 739afffefffe8aa4 18100001000079de
    09:fbdc337ddc2ce1cc 332d67b1cfb631f6 21cb64bb15f761fe b85b1b48ccd03825 5b3de412e8e3b562 6da4762b78707d16 2c63d5abebb406b7 ee8d2fe14eacca59
    10:22bfae144455f22d a78d1b8140320ea7 dc793666924e910f 8ed2d170e58f4b3f bb6c0eabcdb322fd ae93516a81c7f17a 27174afe2cdc225b 2d1cf9cf30618a1e
    11:71dd378c76ec0231 b41a5704a4e175aa e98c0273ee7d1796 6b438577e2d6b955 381f6fa8b0986135 b2b44929f2c1c7a4 fdc9f576ff52fba8 b04a54c8d4722688
    12:89f2a500e089ab32 08322b6b2701fecb 6f234a81e064f6e6 c991de0a6fbc9d10 8136a9f97c945aec 33e0599c2c5c3eda 4c90b817df5dc0e9 c87e07f164309aaa
    13:80e81ef985977c30 7d602b2750742466 ce1a34aa05b7ece5 d814c64ed90de816 76d6216a4e9f0f89 2274272028bb2b8c ddef2bdb52cfcd42 8a3aede686da150e
    14:85023047b28edc40 37070e3aa89ba06c c22f15ab74b30ba7 6d5a3f18239e5fd0 761efc3e91564055 4931124198d3e30d 8e4b097005691930 23ccd953fe1d135c
    15:586290593b473fbc 867d1d2cc0a2778e a1d12fba4572b16d fd963235368e90ea e6674e10533e8b25 581a579f4c0e8192 c2d43bcf9b198743 370b160798f51b17
    16:0000000000018a6f fffffffefffe0f77 fffffffefffd6392 fffffffefffbefa5 000000000004a7c7 00000000000127a7 fffffffefffbc7b5 fffffffeffff1550
    17:9c95bbd324eb0d0f 552c0b44a0be7352 a084d2669813d1b4 4b84b46259dd229c 45ebc7eb1ea69c90 f2c5ef0815f842e1 c233ee134b943a61 c09b97ea031c44fd
    18:0e58f191aab8b466 7a00f16e1e8add92 f6ad1b15783d1506 7312d3d01502d66a 21fe81441fb3ead8 28aa9cf6b9c39ee5 56c91bf29b56cf3c 4acb1d366a2a0ca5
    19:a7bd675e4eb4bdb6 f033883ed40c35f5 1246bd2ac85de200 de0d50a1b3c13f84 3290ea3300889f7f e5a95684f8c5eea1 5af4fc0a5c08b02b f36607460cc04a4a
    20:78861e8557003337 f95fd701d36852ce 91f52064b28522e3 38a58dbd200aed0f 7d3c869255fc7feb ca26d65ba79dc2d9 b0f8235e4a1655ea 37619a2df9f306ab
    21:1c194fe3627ed6c9 9d8e881bf3ae6149 c1344d5b0d4b813d 0d48171aadebefba 11dc9c71cd08168f 52174a2ce11ef524 317b171df7e619e7 5bf9bddfe91eae06
    22:1f03c002f9eb7d25 4ab5d8d55ac014e9 46c4eb8877c5c4c7 4c4f82ce96f6e9d0 c41bb36a90af0954 6840f0f9ffd23140 bd5110402ff924cb 7e088de96fbe6a0c
    23:6be2890bdd087524 4e8c2e1b55c4e798 69b5ba29d37fb64b 8ec89a320b957fba dcd1f0a575a2850a bb24687d64acc8e4 368d647a88bedabc b84b085cfa7acf30
    24:58220001fffde47b c5a3ffff0001814f 76edfffefffc63c6 c991fffffffe27f6 2bf6ffff000092b1 802ffffe0000d03b 8c64fffffffe8aa3 e7effffe000079e1
    25:0f250443696a328d 3d7c618d57987bf7 1ddf8f82f231d2c9 88c3e4ac17f74f76 c2c63d00dea960c0 ccbe0f30676bd0a5 6fb663d2d1814031 5bcdb0de704c8e98
    26:e89a26dc00bf4891 179e71d1f0046cbc 8b95be75c946c721 09cd1f1f979c50c2 6a2e6581797e742b 930f2cb640b6a8de 3ae1463ffbf6750c 45c82733a3afbf49
    27:b8fd6f36333f04bc 3c23abf6ffacd7d5 2ec62319f595d690 e65b0a8953f150cd be465193fab00fd6 9660fa7294b911b3 b702078277452842 0f85db3126329791
    28:89035b031f78f932 0639d496d8fa56cb 6feab57a1f9d8ee6 c70c21f390425b10 845b5604836faeec 3697a661d3a4f0da 4f4f47e8209f2ee9 c6dbf80e9bd2a8aa
    29:ae0108c1b0e42c9b 0bfad9f98888c0fc 82b7d41c1adbb8d8 8f1b07114a64bba6 f8d4b5df92571905 ee436e385064a2a1 e364b7c3813dbc06 1bb9c9a9779bbc42
    30:6d8012a2247df07c 06e7218aea103afa d1093ebc8270ea8c fb6d43bccfd1ae31 6b44548fde7e7cd5 74cdb33ce9067349 102821bc8d56e636 6840fa9b24df840a
    31:96a1ce7e9952faed 0ee2464753c2ef03 77df1de0ae419871 ea0a6c5cb0d9b0ed e5fbfa5a68c61c6b a21085ac8451c72b 969bc9c95d360b07 16d9565b9f73761b

    00:000000000018af79 00000000001bcc20 00000000001ed3be 000000000018fda2 000000000019ebbe 000000000016bff7 000000000019996c 00000000001adcc1
    01:de67665cf012ee7a 24cd3c823f28d506 90009baf0afdf17f 58c52bdbc7b6dd01 92b26dbdb0fd67bc b2e8481215aaf3b3 df9ff710903fe899 cce60e1fb3404aaf
    02:3bb500637615ba85 c191b504208b7088 61cfec9c31d82c19 fea71a1962cf77cd 2755c6a11ade1bcf ea59543a4c180e88 1a2ce358297a885c 3b1f4f5796cdaea1
    03:28e6c0064005a8ae 9c3c09b0a1659195 e0d1798cfd737e71 bdf53ecd2932ec6f d526baf635ec4e3a 93db3315846fa1f4 6b638e124f610f1c daa36777ffb26e47
    04:aa3dfffe4621b743 3f205efae8356914 8a259e73f9011fb3 730ae7fe495d3aa6 8c7fe86f63052c7e f5710a35fa15d2d0 9011d1cd6c763b11 a37660aa9187f5bb
    05:5694838a16a04725 c97dae1395e7db7d 91dcea3c21201df8 b4470e9138685eea fb77e47f22dbae61 13c94e27c6f9791d 93e9f1c6d4fc31a0 e82ee4dc4ef778bd
    06:0ed78cdaa6a8b375 c94dea2bff877f03 8a5e7f5c1b5ec0f1 bce6baf48fed1281 088ac844740d7f06 91d09d75863bf39d f28adc56932eacb6 4e248093b1fa37ff
    07:567d8a0180d034f8 581f71117f7b59e8 b6a51e4c9f6d22a9 2b3af2a4c4a38985 2c4459208bb2b420 f2fcb31452a9f15e 677f7046ed51b095 daffb45b9c3b46e9
    08:dbfe000000023055 ea21fffffffe3554 419c0000fffee86f cf7ffffffffff838 2722fffffffde69f a75b00030002ba19 c17e0001fffe8a56 f325ffff00019adc
    09:0616e2a16a38568f 01a369af97651317 016d3dc65eaf05cd 1028481a9fdc4e7c 7ba29b0e3a31eb3b 7172b99931670bde 0398e5ec75757d4c 32d531ef3349820c
    10:0cf38185d5e1aeb7 0a85550a372ca70f f62209ee5b52d1e2 9644aa4cf489add9 7a91431e7ac74edd 3e52892054ce6157 26d2720b9181a8a5 d7cf70bf51c0230d
    11:635d98b7cf5c4324 ce63b65872c3bd15 320a230bdff2562e 405725ec767ab99e 84c4f3ad7fa613ef 19b33dbe43041cd8 4d53e8c60b6cdc00 bff528f7a1f7f3f3
    12:559c43dafdff7144 bfa2e5c75f06e119 774efaff9d89c3b0 8b9648a1ea01daa7 6fee5efdeb8eb579 09b3f9ee0bc8e3cb 6f446987d033a114 5faa94755d51c1ba
    13:75c6a87f9bed4b38 d508a6b7b9c5d0f7 a6af88938046a3b6 aed1a7204241e9c7 7084eff774642a5b 38e454137455578c db6500aa63ab1b80 7e7aaf08f4358de8
    14:369f65f0938c0036 57ce82c76bef0dc4 b22fa22eaf85d14e e64a2cd198592c49 8a5e2c898d21f579 9208b71a41eab5d4 757a499aa5a5c603 835fceaf9da11f1c
    15:6ac5a8c73de16b2d 5d2b1b3a385787f1 e58f133f2fe46e49 d701d9c553497107 52bac53b7b990c51 b31c9fe9cb8a6396 4135474e8858d66a fb5c675f636646bb
    16:0000000000003cf1 00000000000397b0 fffffffeffff1197 fffffffefffc943f 00000000000442b0 fffffffefffee68e 0000000000027d64 fffffffefffe6152
    17:b6ad04ec6f414bdd b9ca2a077cf03d75 d3ed110e7a1a68c0 c8afd31240471062 909de9baaf9c7126 d6db1e334664af48 c7e6d788a07eef38 c6d5e882d60aa02c
    18:38fd1613e2150db2 55f58e4dca261760 ee72513d86df2206 f2831268fa33f8dd 2b0951447536f3c5 35f1a46269f356a4 573cca4bdd7d7fbb 5e2389ea95c9c940
    19:a930a53136da96e8 aedfde2dcbc7f6fc 66502f87ed9a0555 3e102594f70f3642 6f0feedd7a5024e8 b78152243f49d702 823873f781c0320a a06b804ccdd61c61
    20:ac860001b9d9b543 418ba10917ca2714 8828618806fe5bb3 73c51801b6a30ca6 9088178a9cfd007e f598f5c405ede8d0 92fe2e349389dd11 a08b9f536e72b3bb
    21:67ef1299009c58f6 1e0f31689444e474 66054ce8cb43000d a1d6adc340c1e2ac 9c159b8ff53da198 7fd8722a7b4d4398 e6364d5019e7403b 88c5172003bd0978
    22:7ec0cef76edfd0c5 1fcc0321452760e7 288037e91ac40f2e 4d71a008a394322c a4e8c7a7a40b188b 4a7a14817523b789 9ebc2cb4172fff61 1b46aad720c4e9e1
    23:669992d3b6dcfc3f 7da1e84c633f00c4 b265dec4619a0eb3 5779873f4ba919c8 24414f8fd62d084d 2ec2efe37fd0941c 784bae5a549d103c 43aea33fd99690ca
    24:2401ffff00023056 15ddfffefffe3555 be63fffdfffee872 307ffffefffff839 d8dcfffefffde6a0 58a4fffc0002ba20 3e81fffcfffe8a5b 0cda000000019adb
    25:ec675d1162de8bf8 bb4de6137268bdf5 28e621cb9f498d03 398a99b8be3600f5 0429161564ac290b ad6c0cabd2fb34db eb36971862f30fb8 a2cfe7524a1c4151
    26:64166804d1f14580 0a6367a4de2368d8 5b93b835ebf0f03d 5ce52931ae7a22d0 d7f7a4f6f523f0b8 e60e7e3af526f7d5 487fe04e678db772 c29db5fb81ad00d5
    27:4585359d66c7162c 43641be4d9c295df 01d6c0d038af9318 572cdf9e5081dc88 950c6b02df816e1b f2669c9124b819eb 3537240b67d083a9 5ccff88fa095379a
    28:539fbc2301fbfb44 bfb11a32a0f8af19 766305026275b7b0 8d99b75c15fe6ca7 7309a10614737779 0b420615f43ad7cb 6dab96742fcc7714 5c536b8aa2a8e7ba
    29:4c2316662066898a eab9c378562dc2a0 e5cd33f110493b55 6258bbc8de83d978 91f9875c740a8ad5 dcffbf10e8f22772 3b1c7496a44d677a 7758450eb260be1b
    30:560c3e3956e93804 92a78fe74f63aa26 f8f9a6891a526ed2 2b09782b342cd064 2346438c5ac5c218 4d0096f2c2b65d52 1882ad59b002f613 df8505e48fa45ac5
    31:552906cadd695c82 2d57d14b2b4178e7 23c362bdcb68dd68 40504263b52b7483 c18f89891323346d 81855d8b368586a9 47e08b36f15d2194 7df937b716ada7c7

    00:000000000018aac5 000000000019a395 00000000001aea66 0000000000193b4c 000000000016507d 000000000019794c 0000000000167ec4 0000000000185992
    01:71404bd6b8aa3e2a 1b29371e9377e305 090258c85e249f8b 963d2cc11b4a3f69 9c8e1380a215edb3 b49248f493cd131a 4e17965ca9d5a383 aa26ff66eb07f515
    02:c6eaefa5aee1d898 64a597b301b124d1 9612bf7c785a5133 4474f9d90ee16553 2c9edbb008fd3f01 18f52e16ebdc2dc7 c6b1ba190293e198 01ed678c48616b51
    03:56add8abf34184fa 9bc6a813ea76fdb3 4e7ba856c350bde7 4822e94b9b8bba21 c7f1e12ddcaeacc4 a3318062c61015a6 56b794518b8f8c2a f851315d6dca0fe5
    04:88bc14c6590c183a 30b1a789a82df6a7 030b359f69952286 fd3c72bcaa61e2be 15432e48aaf2afb4 e09b7d39be48491d 822979f2e491b767 69f78e8b3d5540a7
    05:b2493f796195c978 3ee5adff19911933 c1502fa54e5c3bf1 687f1ab16534df74 2a83aa1417ba43e6 e8ddbd44d67d57fb ea1b62d53bf6c473 bb6be829015a4813
    06:e1a07268f75e39fb d0eb5f03ca619ca1 c69fb740ed6484ac 62eef24f1bf29c2b 842c7b3d8d57c2c6 fb9934700a2e44b1 a8ee73334854a361 6a4421baff03f03c
    07:f41908f3668e3b13 d42c3588e83b2199 e7aa23207d2bff71 bc84fb5b78910533 c8bf6995bcf5bf92 9af8ca80e74b482a dd3bf651e1d41a6b 47b638c9d1111e96
    08:bcee0000fffcd8cc 46b2000000038e27 82330000fffda278 a74afffdfffd8b5b 44aaffff00003359 a1220001fffee0ca e1b9fffbfffdd4b4 eabbfffcffff6945
    09:fe987a35f139509c 073983a7c45c53d9 fc8148c84cd3cae5 5d845979c782bebc e0c66a7c59f9c4fe 6fe813e501abf1f0 29e36988e7bd42e7 ef482c13459fd5a2
    10:dffa0abe70dc07d7 c99d7f8eb8786793 5681f4732b1c4b84 ed5ac68493523758 681385c514333aa7 895183a10d3bd106 38af041b4b389743 0a33cb5f382de157
    11:a3ab1c7ce27b4d27 0700014166cecf08 33958babd990339d fc5cc4e547e25edf 287a9210a8533eb9 9031c9dd624101d6 da105d98f518693e b6e3b6899acfea08
    12:7aff5af40f36d037 d0ababd2a877f4a8 fe006a6b345fed85 041bad9a7340a9bf e910a90a2fb534b5 2056c1b77ec3071a 7f37e76d7a094b64 913e38a9937496a8
    13:4904f0095394e1fc 185f1193670443e9 3d0628f19c66b029 6401f899e1b7755a 3f5a3aa9a7f83a3d dd82c0491d75605e 9baab5e8f38a3fa4 6265c83932c95c84
    14:e48c91fc576cbab5 f37aed31eca7c2ec 962903be9a88c029 3e85c221e40d757e 9af0cf6808f341b0 93d8fb3e32ab45a8 a118ba39850b100e d649c340f43960e0
    15:15fc8fa95f0aa6e8 b20d536d50b58884 96cee00990e1c56c 3eca5aeeb5444310 d1d954a8252bb6f3 5553c9762c9036f7 730d27a557e66673 cdc53c0ff1da390b
    16:000000000004e4d9 fffffffefffcbbca fffffffefffd9e55 fffffffefffe2ceb 0000000000049f83 fffffffefffc7479 00000000000092a8 fffffffeffff1d37
    17:cd5ff1e4da82ac64 83a760ec1172d179 7ac022d9e2ca9293 cf806bae7a733790 361e8717cddb73ac beb879553a670645 0f788f6752e0a612 76bb84535574b868
    18:929aaf00637c7eeb 227b0cdb22467fd4 a5298f25a3107076 d0f09b4f13b15008 412f65c452b857e2 8fd368b5a00b1d34 fb93866d90520693 cb5ad00b2702472a
    19:703c1eb92b590f14 60b47cf3fc052e1f 1e3c1f04e62f7bbd bf74cf7d97e82e6a d1dd1a8432110ffb 62903d6a54ad0312 95cc5df60905a19a 23ab9d34d30d57c4
    20:86d5eb35a6f23c3a 2d0a587657d546a7 0234ca5e966b8c86 fa058d435598c8be 172cd1b755090db4 dd2e82c241b5451d 7f5086091b6aab67 6ea27174c2aa62a7
    21:429f7e3b49b298a0 2c9f8ecfd6bed849 84844b538363c70e e6e80817fe18bfc9 c154048d83f906e8 e5004b928ae05bc4 85622738858ce751 39c32bc824b88608
    22:c7209f34a1f8a789 a889c901dd118808 fd59614f5ff570fe 8c40371b76e68734 0fcae112b134b818 5c8457a78b04104a 947121e7fb22f2c7 caac4ae4315f8837
    23:7e1eaa2e19d3ee88 02a0799de9e98277 e193417b6aa4ea08 463417ef7c6c3c66 c764ab1b77cc7e4d 9173d098519a218a a4994f919f1c889c 0656d81073487595
    24:4311fffdfffcd8cf b94dffff00038e28 7dccfffdfffda27b 58b50000fffd8b58 bb55000000003358 5eddfffcfffee0cf 1e460002fffdd4ad 15440001ffff6940
    25:b85c3a8a43938f3c c451588418b88d32 3cb2302a9e0fc0f7 7edda24786f2b131 087af8858d95f0b5 7c56d70e9b8ebc1b 815451e0042ec5fc 97014804101bae40
    26:7a4c56997cc187ac b971dbe423908913 e795bce8b9731225 212fa4554a1ba603 051e38c0901edc3c 1f45e59066dca5f5 875fbb5921e3d4e7 c5dbfcff586e3924
    27:9e58ac1fec73153a 94605fb4267c8ab0 a781e125705914b8 b9ea661a18754f7d 87e2cab93b4fbb9a 0c57e4e92dbeecd4 2d30977c5d27b779 879414ae1c803fa5
    28:756ea50df0c78437 d198542b578b48a8 fcbf9594cba0c185 04a252638cba01bf ea7f56f3d04688b5 21df3e4a813a871a 7f4e189485f31764 9627c7546c8b0ca8
    29:bf955fcb392e6651 e2783d6526aa4824 31bf677c6607f2d2 8f1f5067d6c4fa4f 8fc0191664d18207 b305899515bf0b4e fedfdeda62542f12 4c072bf710ebf6bf
    30:bee65c620f384acf 88dfeac26be5adbd 2c89e3ae18176980 ae5b146d8919f9e1 f617d44ab887f12f c2a978a838222753 9f33b0ab377fae1a 576dd025db62f393
    31:7bc5fd25330fadec 0e92776a695c60f6 e694872993e53316 7bf5adf8c1efda3d dcf63e25b3ad7241 1e042ee0efce3eb7 0488ab1440581501 3ef11950d1a4f41f

    IT WORKED!!!
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────────┐
    │clock             ││╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥│
    │                  ││╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨│
    │clear             ││                                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │                  ││─┬┬┬┬┬┬┬┬┬┬──────────────────────────────────────────────┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬│
    │data_in_tdata     ││ ││││││││││0000000000012D720000000000016C270000000000012.││││││││││││││││
    │                  ││─┴┴┴┴┴┴┴┴┴┴──────────────────────────────────────────────┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴│
    │data_in_tvalid    ││───────────┐                                             ┌──────────────│
    │                  ││           └─────────────────────────────────────────────┘              │
    │data_out_dest_trea││────────────────────────────────────────────────────────────────────────│
    │                  ││                                                                        │
    │start             ││                                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │data_in_dest_tread││───────────┐                                             ┌──────────────│
    │                  ││           └─────────────────────────────────────────────┘              │
    │                  ││──────────────────────────────────────────────────────────┬┬┬┬┬┬┬┬┬┬┬┬┬┬│
    │data_out_tdata    ││ 00000000000000000000000000000000000000000000000000000000.│││││││││││││││
    │                  ││──────────────────────────────────────────────────────────┴┴┴┴┴┴┴┴┴┴┴┴┴┴│
    │                  ││────────────────────────────────────────────────────────────────────────│
    │data_out_tkeep    ││ FFFFFFFFFFFFFFFF                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │data_out_tlast    ││                                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────│
    │data_out_tstrb    ││ FFFFFFFFFFFFFFFF                                                       │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │data_out_tvalid   ││                                                          ┌─────────────│
    │                  ││──────────────────────────────────────────────────────────┘             │
    │                  ││─────────────────────────────────────────────────────────┬──────────────│
    │controller$ITERATI││ 1                                                       │2             │
    │                  ││─────────────────────────────────────────────────────────┴──────────────│
    │controller$START_C││                                                        ┌┐              │
    │                  ││────────────────────────────────────────────────────────┘└──────────────│
    │controller$START_I││                                                        ┌┐              │
    │                  ││────────────────────────────────────────────────────────┘└──────────────│
    │controller$START_O││                                                        ┌┐              │
    │                  ││────────────────────────────────────────────────────────┘└──────────────│
    │                  ││────────────────────────────────────────────────────────────────────────│
    │controller$STATE  ││ 2                                                                      │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │controller$i$clear││                                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │controller$i$clock││                                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │controller$i$cores││                                                        ┌┐              │
    │                  ││────────────────────────────────────────────────────────┘└──────────────│
    │controller$i$input││           ┌─────────────────────────────────────────────┐              │
    │                  ││───────────┘                                             └──────────────│
    │controller$i$outpu││─────────────────────────────────────────────────────────┐              │
    │                  ││                                                         └──────────────│
    │controller$i$start││                                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │controller$o$flip ││                                                        ┌┐              │
    │                  ││────────────────────────────────────────────────────────┘└──────────────│
    │controller$o$start││                                                        ┌┐              │
    │                  ││────────────────────────────────────────────────────────┘└──────────────│
    │controller$o$start││                                                        ┌┐              │
    │                  ││────────────────────────────────────────────────────────┘└──────────────│
    │controller$o$start││                                                        ┌┐              │
    │                  ││────────────────────────────────────────────────────────┘└──────────────│
    │gnd               ││                                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │parallel_cores$i$c││                                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │parallel_cores$i$c││                                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────│
    │parallel_cores$i$f││                                                        ┌┐              │
    │                  ││────────────────────────────────────────────────────────┘└──────────────│
    │                  ││──────────────────────────────────────────────────────────┬┬┬┬┬┬┬┬┬┬┬┬┬┬│
    │parallel_cores$i$r││ 00                                                       │││││││││││││││
    │                  ││──────────────────────────────────────────────────────────┴┴┴┴┴┴┴┴┴┴┴┴┴┴│
    │                  ││────────────────────────────────────────────────────────┬───────────────│
    │parallel_cores$i$r││ 00                                                     │FF             │
    │                  ││────────────────────────────────────────────────────────┴───────────────│
    └──────────────────┘└────────────────────────────────────────────────────────────────────────┘ |}]
;;
