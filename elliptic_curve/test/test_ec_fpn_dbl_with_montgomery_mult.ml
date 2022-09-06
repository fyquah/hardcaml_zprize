open Core
open Elliptic_curve_lib
open Test_ec_fpn_dbl

module Montgomery_mult = Field_ops_lib.Montgomery_mult.With_interface (struct
  let bits = 377
end)

let config = Config_presets.For_bls12_377.ec_fpn_ops_with_montgomery_reduction

let%expect_test "latency" =
  Stdio.printf "latency = %d\n" (Ec_fpn_dbl.latency config);
  [%expect {| latency = 154 |}]
;;

let with_waves = false

let%expect_test "Test on some test cases" =
  Random.init 123;
  let waves, sim =
    let sim = create_sim config in
    if with_waves
    then (
      let waves, sim = Hardcaml_waveterm.Waveform.create sim in
      Some waves, sim)
    else None, sim
  in
  test
    ~config
    ~sim
    ~montgomery:true
    (List.concat
       [ (* Handcrafted test cases. *)
         [ Ark_bls12_377_g1.subgroup_generator ()
         ; Ark_bls12_377_g1.mul (Ark_bls12_377_g1.subgroup_generator ()) ~by:42
         ; Ark_bls12_377_g1.create ~x:(Z.of_int 2) ~y:(Z.of_int 3) ~infinity:false
         ]
       ; (* Randomly generated test csases *)
         List.init 50 ~f:(fun _ ->
             let gen = Ark_bls12_377_g1.subgroup_generator () in
             let by = Int.max 1 (Random.int Int.max_value) in
             Ark_bls12_377_g1.mul gen ~by)
       ]
    |> List.map ~f:affine_to_jacobian);
  [%expect {|
    (Error
     (((input
        ((x
          0x16af2a848e72dbbc1a7457e79c29ee2655c37f63beb8e71aa2c1acaccf7bd621ebe95d70492f8a5fcb27ee95acdaf1c)
         (y
          0x141cadf36802f9ec4712f106c09d72967f0fe3e2d83df70254e5fc638cd57fc7a1b46eb32b2592163516f257f00ab2f)
         (z
          0x151e2d6583c8da586b102daf27e3a74b9eaf116782f26872e60737658e446b4c586bafd5ae7925727b37fdb85a0a4ac)))
       (obtained.affine_point
        ((x
          0xbf746effd17625b27a4d1349219733efe1b0350afded407e09fddf865286f7ed95ca5c7e4284d4eabc9921cddbd00d)
         (y
          0xe23811a1689cf758b4711df9d52e339c6e8552edd030aac4fdbb50461f89a9c1e3116b8475fa0b1d6a4af91e361880)
         (infinity false)))
       (expected
        ((x
          0xed453141939e91056edb5a4b5452ed7e61f7f3dd2a4b7ee90e97c9a2301955880661656781dc90857aed6d6a416390)
         (y
          0xcfb0b9717bc8e5ae04601813171337ad99cdae42c561cae80b12f135c64479d6a23f5675ed5ca7e2dd5e8727d7c7ed)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0xcb83df20d567a1486b39aeda2071fe17a7fb4bb0fdb24839459a75853c16b670c6b592a15f05cb30e7e89570dd20a1)
       (z_squared
        0xcd6419cf1300b645725a7de5af93b28670313d7ee87c415259dd4161de47f74861d60d3ce59eed7b346551c7116d02))
      ((input
        ((x
          0xf355ced27734cc29a644282a3e2fed1cb7af316d0a2ca48337d7dafd03979cbafaf4d3b6c80496947a2c95b178748e)
         (y
          0x61dd5c93321a31fbaba0fb499c39c6580163be4339be1191eda5cf145ef68478140d1457b053803b7007f352ec02bc)
         (z
          0x16d8feac1eec67193e34fb6b36d26dd4e5e9920ba1ff0b48a95b1607518a71355c65889f898c8dedcd32db00fac8fc)))
       (obtained.affine_point
        ((x
          0xe8dd3b684850f3382d20a9d3ec0e3cce0306f3bed6b04cb961166d0f4abcb08fe30ad499c85240d6b06ca7a09de05f)
         (y
          0x180e672f659cc37063c6f10dd25920e770c382bcda4bba611ba57903ed7b9e24820722ec05d3610bd5ce936b9a80cee)
         (infinity false)))
       (expected
        ((x
          0x166fc4af4eb11c73b020ccb445e29b2af907e91df90a56eed8c27eeb35753cab004d510b553718c4c5ab33b43704669)
         (y
          0x15812608eaa71de25ceaf0af1d1970b13ed55b5cad864fd5feece9882022f4b37682c0e89cf40270422266144f74a38)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0x565e70853c2c0e6845a7646e6502cd46745dcae8f766b797c688afdadcde178da0f380742e8bc9247c4a6c2e24a7bd)
       (z_squared
        0x48be7af588f005ef0a9222632943c2433b9d4f469dbd0630071ffe48243b88a49580050f086297100329033f0f87ec))
      ((input
        ((x
          0x5bd675cbb9f2d96d10e65c81d8552f3e0c6ecc46bb3ae51bbf121e997c78bba5b43738c8bd565262d64d36eef51575)
         (y
          0xd7ff3d910ced9f4dab9b0e10dfee96c2c50aa7aecd4484bb3172bfd3f5a89038bba4a126201533f350074744d67337)
         (z
          0x104455c198fd492fc4ffefee56f615b768fbbf07cf3efc4e9d2537e0e5d545db4ef03fea10f84e84353f9e21d8fe3d2)))
       (obtained.affine_point
        ((x
          0x8ef9343736347fa4cc2072c772d7f0d58e073228256fdb2599276f806dccb794d76f558fbc3e457153593c5ba5a217)
         (y
          0xb4eeebfcc87fac5708c1eabb62d5ac31717722e730fc34f8a830fa6d313089b586cc44937d6eddc9f0efb9ef64512b)
         (infinity false)))
       (expected ((x 0x0) (y 0x1) (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0x456847c763d7bfed19efb36a15ca67b6c20fa8fd0aba1896959a7199bf8b668774cf71be7fdeee1694adc28f8ac19c)
       (z_squared
        0x1ae7f40d1df630a4469ea7a6b3b36dd5045d93e4463cdf518d9886c6b81f51b1ee6f6153e19b77321759bce57a48a8))
      ((input
        ((x
          0xfdf1a4bf29db1e8cf3cdfe0098adb0fa2134c1fba3e8885cd284a53b8e06a27b85371189cb8a203de89c60dc63180e)
         (y
          0x118a011d902a4a2edaff28b77820d93d183b319bb6f481758dda0d82709e8bc1c9a565e34a5b275ccf5ed6ebbd656df)
         (z
          0x90b994cc08737e14195e137a9fb1e1fdb204f112d950ef82c62c33bb6a36037b93540bc7b50fba60d0f3f60b5d7a05)))
       (obtained.affine_point
        ((x
          0x127f9b1f325b74521969b7fc7461d78716af78479d5c4dcfe85727f72a973e4d88d46d2f88ebab7edfe825466fc029e)
         (y
          0x902017179a8823dcfae5b6aeadd050cc53e8c46962c89ffff43d38fd4349cb10fafdd2bd32fd236ff503a7e892a31)
         (infinity false)))
       (expected
        ((x
          0x875effdf14807cd7e1edfb648a1783cfe9843e9cc9b2c9a8d02100b6b7db8c5bbed3a2bcab067585b1c81cdab909c6)
         (y
          0x1ecbdbf882751370d5eabeb5e47205aba2119901839d7278f5e692965c67db01cba10f7b844d7cb548850dd36b41fa)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0x1525b72191a2b7f7b5658f163bb0d39c23a90a5698042c37419d9374fc169515ad912d9568019c18bbff76de7e2ac6c)
       (z_squared
        0x1961cee57096305aefca4d196453cc2119a7fe7bc315eb8c14518adaed09cc29e3a6784335f0c59fc8203c24a34e3b3))
      ((input
        ((x
          0xd1c2821b34a722269e0eef8c4d2f6831118cbb218025b04cf7fb88183fd1dca8448a6566e3d574918b97664f5d107c)
         (y
          0x162c366e9251b3633970b3a94ac4ec9a89fdb8ea37f1db85c196b00a10d7191ea5ca0f57451cbca2eecf144c9a58686)
         (z
          0xc23c1a9eead589d2a2cbeadc267d39ca05231abe8a8408ca3713994761973ac5d1065fea762ffbebb852b292ba0108)))
       (obtained.affine_point
        ((x
          0x120b925cc16afb1ce7362e149cd484d28117159967f11f4df9cf3c5f21f2c4e148df669e2d6c49d55244da358d242c4)
         (y
          0x1875e86abe13e88d0e9cb3118074919595ab0f8d5e2e3157ffc804defa06c739bfca7464d6dcac1a2b55c85dceee779)
         (infinity false)))
       (expected
        ((x
          0x186be938396e55637dd7fec3cf68732e57611faa63b12ad1bb21a394858444efd62055580139b163bce7983e1b7c57e)
         (y
          0x1a0ae46e19c07064ff3f77bf401693750a85f966bdd5dc4d79ff1067de267545a8f7f6631caa58afac940f98e46c659)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0x98484cafee44735e026231b0f73bda968687adc951ad5eedfa057fd6a88f5c4a430f262f8c19bc36c98587c4b0a042)
       (z_squared
        0x142c32931a4df327f164a2066f765ca8d3623886a9a20da018b85423a669a6a9134a37931dd15301c9649858a34cc51))
      ((input
        ((x
          0xdb21c0d4c98f3ddeb32a3c600c1b900775f54719e874e1d9a9998b2103e3f88f8c21f704cf64244850f0b640d12baa)
         (y
          0xb6b7b9501e06b74d6f239f4c59d5df82dcf7005a8155d37c58ae5008781a8121c702c1669cbfbec554cd0d3e984090)
         (z
          0x77e4de4910a2ce55b2df263d303cc98df26ac71f353484d225c8db32da1e3454550e2cfbea2091f34b189c5523b670)))
       (obtained.affine_point
        ((x
          0x3b3573505aef8bcd0c208907fa5e38922a40c20a36ca48b91d4db79060e517088a9fd37e34e15cd08d9669dc131f92)
         (y
          0x13fbbbf7ecb4ce59fbab1353fbbdbf733142bf07c5af2176b3f9e6dfe4e50bc58224870ca185cc3b5205ce445450bf8)
         (infinity false)))
       (expected
        ((x
          0xea9f551221620d8ecf3cedb95490730997cd7fb782fc2505baf49b31e71e1c7ca2aecdd62ed08d6b2feb31b0fa2ecb)
         (y
          0x189023bdef9d16168137df13833f0dd8782c2dc1f02e5c4190cee9678036fbfc71d40b1fa8280c98f7a2504b1974730)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0x15d76bb9c244be3f63d536bb741a0638c63e8c636286e3dd4fbb99dfabf2c4cf5733b828c8f574fe61c42a61075ac7)
       (z_squared
        0x89382a2fce2ceba5e63ff582b5913d535224fc4dcdf7b31524f56f6885e64002a81eec24f7d0d4ed9d3fab5430c2bd))
      ((input
        ((x
          0xbb067a994bc4bf192416ba021d5e39cee45275eb1bc1268e190dbf00c180be1eb346735445557e6693a31d1c2aaaf6)
         (y
          0x61385a7d16b85da0db2fec7b9689949b5cb9dbec4f8bf5d4be5801f2a22cd31455d64957158a8244f1dadc11d69c74)
         (z
          0x989fb459abcf34d0eac6e049ed46fd0525f4a62759192ea8ddd70e67110d476ba85f9dcc41e94117d49eb02d47edb0)))
       (obtained.affine_point
        ((x
          0x18413d5cd4d6ca6facd1d54d8066e579fdd8187d8a7274a4f1303b701aa0a6d9f72b914817b9c9b40b09f9580af2539)
         (y
          0xa2138e193c29cda6dcbdededc44613443d7c5043268f7508588aaf9f7064d08dcb0a21408541952aff9b38f77f4fce)
         (infinity false)))
       (expected
        ((x
          0xa822b68dab5d978eddf61ae38d7604a3aec74cfd27bad8fa4d54667da8acd38fa0f007bce64d0bbae8decac9287de6)
         (y
          0x19e94327ba22844c8b30c1556a07f0e3faabc590b539132c0e35a044ade06688c6ec0886cb6691a033462eefca36b23)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0x50d10d055e6162abaeaf05342141c5e7f6b0dc70ebf4c9d76827883f52c3cb528a5cb1bd753c78b95cc2bed39ca720)
       (z_squared
        0x131337c419c36008bef073c19dd9fb0ee47ee0fbac2564491289df913a6a0cb772a28b1250db5bc37f6b0039e4cd9ca))
      ((input
        ((x
          0x1747ba688a2dca58ac9bd9868291a6f4268aad0b6bd68182dff1f9f7f20685ae6f38e39bd5228715c601f417bf3cc6f)
         (y
          0x12bd5b55beb79f691a661760899aebf82cfb8364cc19103d795034e672e161b77a9f90855e2258f02ffbe85d08fcea9)
         (z
          0x1a6c0885d158282e09639c9eaf296ce45595c91b90d92e310a1aa8a17bb22cd49c63f01f2ba1883752528b3a967756d)))
       (obtained.affine_point
        ((x
          0x2ddb1a77e4a168286aca095b3f942761a6076f27b659b05ed5f8f9022d6ffdf9da0723267409fc223255958cff41bd)
         (y
          0x111819d24ddc1dfa3e9950cae22a9059b907b27d0e1f20542a63502c9d1809b1db93da9c15a5867ff42921e063ab73e)
         (infinity false)))
       (expected
        ((x
          0x10c8b0eb27ba728cfec9b7f39f15a61ab7a8ad19a1cd519f207a8853f50633695df4915f8cd77881d41c5b7d4e42b01)
         (y
          0x407e29ae2a91c7246436842897a668f0cac31b19c36ec3307bfb70f1ff5b56fba93b04d7d23e3ab16f2245ffc1671)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0x23cc120e109817c191f8fc534df53e254217fb2bb9e8b7d46e5579a64ca6e07f4f94cc2c406521116142702f4b31c)
       (z_squared
        0x7c06bc991c2d819d2c5a97bfed7f62fab66d999ef822a6cb9629713a8d90b8e3a26d0b621ee225e58feac45d0aa04d))
      ((input
        ((x
          0x7fda198e96d2fc7f7a4166028fcbb1d62f62bb98f0c72283991a9ffbfa017c1aff57a1bfe8626e60176884fd18d8c7)
         (y
          0x172c94c349103fd0116f8c9bb2cc7751f0416974a8d44cb7a6e1f7a06e602608d6655dfd4f28dc96cdf5883cac8d595)
         (z
          0x1ae3a4617c510eac63b05c06ca1493b1a22d9f300f5138f1ef3622fba094800170b5d44300000008508c00000000000)))
       (obtained.affine_point
        ((x
          0x19e34c6c9e42c7ce49e1da3711ba0732aca093b5801113d9a5bef9603ee0db83ad98b188d9adba57e6ff7cfb52f14b1)
         (y
          0x2ecbcdc67f3965e000f1aee5e4554c45cb6ec33f6994d0aa3a851e12072631ad736d63cdb186762ca02af64b959585)
         (infinity false)))
       (expected
        ((x
          0x17c82ff9d99ec99027ae1c11f2709fdc1028caf99557513afeac5ec6a4cdbad1160b3b7f8132b76c925e4a0aeaded58)
         (y
          0x84ebd32a4929518eb0a373e037b0d2ca327ecd903d1d6e42bd19868c79bd2f8f7fb8d2a00bbe0cbb14aef2a9358ff3)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0x164333eed625f121ac5341647bd5b33c5d247477dcce4c73112e3cd04746a1e314a5d6630c3045bf87971574fbdff)
       (z_squared
        0x188917497f0ca52556686744e758b7de0f90434891a62e08317dd16c579e03600407c0910b7ea8f82019bf212500e31))
      ((input
        ((x
          0xcd68d36af9c45d7b201a58c46d20529a7617c0a8935e7380fb54adb8c375264d371444ae0a77670d0392654c0809c6)
         (y
          0x15be12879751db74489281a50b756fff1a52814c3771ecff63b99d3dbc0876e5f2b78394026e4b121194ae0e7c1f562)
         (z 0x1)))
       (obtained.affine_point
        ((x
          0xcd362087f7eb2d116baee15d7c05b2f08f42f92b10cffa86de2110be869900e4cd4fa34d8885ac7c58e6a60ad4311b)
         (y
          0x1557651ddce6dd656c4ca3480473703abee01faaa1ae011a40e27ed1c9266788d26ce9bed1a12dcde114479c7028a01)
         (infinity false)))
       (expected
        ((x
          0x11a82763cf172653103c34b69b221b9f2f03e5615a7da54cf037da65f19b19b4dcb17ae6500c8f53abab5b289bf4bfc)
         (y
          0xdf440f9d656435d1434fd272eaf9f0278fb64b3b1ce6566f5c2f0d308de402caf72ced2fcbffc8d35494ae69810f7b)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0x159daef55e2a4544c2cf3e7b5199b39b36e278cecf709a822f9e1d72b12eabbfe8812fa6d9f19e97dae466371ba2223)
       (z_squared
        0xd098541e5763132c03b227e2b9830047bc522443ed3fa6e0f136131607fce2de9295fe44cde016a18a9709758cade4))
      ((input
        ((x
          0x208e745ff5ccd18e5bbe0f8653c3104b5cafe04304177bf98f1c85bc450a979b69e5899d96095dc1143f0f936a130)
         (y
          0x142e84c8aa5680783fc7ed6dbc1e8fcf0e3f2d81fe5ee6ce5e8dd46b389a4b82e023707b15be3b10006d21edbd5072)
         (z
          0xe8c166905935e62b3c53e9428f47ac99ad972410de3dc5454650c642d662e604dc6368ad09a4f73c8527ccd3f74759)))
       (obtained.affine_point
        ((x
          0x1a8424b28ebd449624c123d53d87d090cf1fb99a3e54f229126ab4a1632ed02ec8cb73501f4aefcb55e958822175f03)
         (y
          0x2cddcf5a9dc4169b99a831dd65251fa60e202d79587a6a228b915773c029f5c4fc3946fea6000b2fc47844d9c228e8)
         (infinity false)))
       (expected
        ((x
          0x1180cc11ae4a62fc1ff6b6d53177e2c9ee9df936eaf219c407a4e2c1cb99368d74b3e09769b6d2cac4baa04de94282a)
         (y
          0x12551f974c18b3c45ef263867d6c51dcafb44652a8079ccf7cce23d032a44c33ea5f09306138efa90ad01299b460809)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0x12e634e464c65f25d1394897546341191669537c3d400254d7679514456c8195c22f282aa7986a0cdb347e2879e8938)
       (z_squared
        0xf177e52a39246036b902ea01380b695310505cf9f8cf918cd80ed95770487f35c6b05ccb1f46f80c6d62b7f7a254ed))
      ((input
        ((x
          0xb7ab4449a36fc964b8e11ea942eb8a22703e7447577cffd34f1be0c7b0d3d796364764291a59d5885400f16f1439db)
         (y
          0x619a18256ff17d7fdaa1c04d909322b4597708e1b15f932d4ad4e03940a127c691cb086682c2bce1cf0320363db4fe)
         (z
          0x19ff9e36bbe645a1d270e0a80860a88f4a4cafd863cd57e39a0af02b9012d8fc521e75c535e412d44f1abe6795b81f3)))
       (obtained.affine_point
        ((x
          0x161a0f377c93dc45324c0fd2059cad1d584007fd6d6c27da67e258b6223f1c196cfaa52661e40ffac5c7c108b126534)
         (y
          0x47e99d4ffb82a12691eb5da6c54e1d53f3fe998387d35a6fbaacbd0012fd8cd99cd0e8e3202a0d045d9fc283f11a34)
         (infinity false)))
       (expected
        ((x
          0x65a6426890f4c0f4904acfae3ce7845de2d69dae9ad728eb417f56d23794d54bd6826816ca6af67b037659edea3274)
         (y
          0x1a21d61f3cd4dad98f54e2074f992e76ac8722696598a840da89639fe1585d1748b086e3bdc0c22999333b3898a82f9)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0x1807edf94682f5ff441f34b5a665a9b5a827fe660a53b5eaf8bef7630b5c267a90d1b312243f12723e039feab8fe4c0)
       (z_squared
        0x1407a306b7f89e737ff8b35ecd95eb125d6bb298c7574568bab9955bd4f5c556f9e3b8209813915c60549cb340aeb6b))
      ((input
        ((x
          0x1a47629aebebf5300acf1af5fa85bae19c3be1959c1184d536cee3e8954eb4f554dd4690560c80882c328abbbeab7cc)
         (y
          0x3cc6e9fe0af949ec0054e7f3c174c151824e6ef7d060ce0cd76f56d180b83fc3b196334731bbb52c74a3148aa6c35b)
         (z
          0x659c86bd7921bd29e72c2654869722bc6e1acebbaafde3c3cf43a209052507dc69f45020661bf019114189715625a9)))
       (obtained.affine_point
        ((x
          0x1680ccde122018467671c49f9d573adceb24875d938b68c2b3a93366d2928adfbb373050ac826606737ee1f8fb19ad4)
         (y
          0x7cb43acbf85f34958a8e99640c27e39a455a7922b65e67c2c1c15ae12839158312b9a8fa1d3a1530ec67162a2caee5)
         (infinity false)))
       (expected
        ((x
          0x117a0b02157fc3e1578a9635e459dfa74fa8dd926494de4c214dc22ba1b03c9534a82e314f9e1efd585525dfe4d5b84)
         (y
          0xc1164468af3ac0b91b5608cd23c80fe58e3a7a1f0b9681c5faf83a078622021ca7df7c0b061c46f62535b4f7cad415)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0x9233cd8d029b8d554c38fc305a8482bac30fb5fdb1d532756e067490b3f07793f4241f309d5a55f5b37255ab229db5)
       (z_squared
        0x79e4c9726fc00a871f601f86cacd2b744b1f2ebb13c0b2b92cca4814ed34395777ab49597ff4f36b7683aa6effd89e))
      ((input
        ((x
          0x7d2a2e055c6291bf5c6f8726cb596fbd6a795f772fa1f0dba886ab206a3ea152b1293cc0b1f2b0f7c40b4ee59099d3)
         (y
          0x171b972fef5eb90025812e092ee8813a528da1e049c74cbdb81cca593b97c765df77dd996bbde67c19f1ce6f96dacf7)
         (z
          0x1a9b621f1b277f28e3cc5e0392e0575398d43829d352bfd27bab010eebee3168dc1e341e1db2d8afcb50ac1b95cc671)))
       (obtained.affine_point
        ((x
          0x54776fe0e3d0c300a9a78559dd4ad8a43321ef18be85e5049d265217ff395a24a7b8a3c17af540251b12a62ffe1354)
         (y
          0x149c80da178bf571246290b8461b587f379c83e8934a4218e6f8828269536e6c5e2badb76886395c3b7adb6b9518608)
         (infinity false)))
       (expected
        ((x
          0x1529eac6bd842278532c78d45d06414d8f23d0de2a12a69d79d2b96b9a6a31e9f236532fa930e27ec8f8052346a02a0)
         (y
          0x12982ea3680a6b102f90d420d3daa37e23c257a2e9c14539df5674c3f3a7d85785d26f577a2d40a3f8854c6888accf6)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0xc6ba8f8f66a9dd234d09fab118cc46fd8272fde6869a8b88f2f2ec3d804a7054dce9fe799a9000cfdb2b91d3f6b2a6)
       (z_squared
        0x2cd8c3c1ef420568a89dbc59f8b630df51b46eb32a749fc0dae902d92efd437bbf3f738bd789f33e4ea5264a535f79))
      ((input
        ((x
          0x2d7985e8f6bef71e190005c7df3bbb90a417e647a362a2762a5967fdc9cec8724d30d979b72b6fdb4c632761d19a6b)
         (y
          0xfef890d2a1373bb6c227d9826d1c462679cef97b668265c3216a1bc171b2d00b2417e4f5d521ffb1eb28e0dcfb8f79)
         (z
          0x13914ecf06e93e7c659273ec3f445ade4771fc3aa9a07a187b910d66753b2019d6d0da20160e93ffd58e31c98256b9d)))
       (obtained.affine_point
        ((x
          0x840eb50028cbd299e473a16f421865f5f2928eb193adc0526fbabfcafec1c37c846334e6b050f1efad441e6f2f4be8)
         (y
          0xc756c15a07a74f9e2550ac7b7804ff284be321cf3abd60edae0a030c8bda20fdf386caaf8a7a62c1ac3f9b717ee58a)
         (infinity false)))
       (expected
        ((x
          0x7b4e9334a8830ec2638e1f3832f9b1830a401e116107a8996b8da058c848dafba4900598762ed812cb3ddc0abd9c38)
         (y
          0xe91fa26a0fa8bad5e30c80648332a7bf8a1286db23fe771348bb71830d5c5c7639153867681f0bb6bb288cb8b14031)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0x5ce2368c4f5bcf5144ac2989f88d61d34f262c533bce5e4a5973f514c012033819a54446e071bcf1dc3b6ce3663644)
       (z_squared
        0x189a650d008574e3c9ed1b70e15d8b1d65fad06bf4ddeba3c501f3b7856b8d215032854138f2cc8b0c79b1f5c068c4d))
      ((input
        ((x
          0x73f4fb25fe1db800590128a5cad5a6c6c5392c64f25b311a05f224597904142f98c221e56ce98ab28989c9aa44dfb2)
         (y
          0xf08c9c0e390e93ef6c9ee86c1a233a8b00bdeeb34115984e64d403a64f7d6138970294704b8f761c55460baf5fc4a5)
         (z
          0x1c9faf5a59fdc23e61949aeab3e1d3f3845c3d23d6f19a6be55c6a8b61785725b8c73fd6c9982500df3a5a66bc638f)))
       (obtained.affine_point
        ((x
          0x1a134de1ff759aec7801c9b3c63632a5f64dd233ebb982096fefdb0b15d414ff6c19a8741227b9c0766d584ebbe5da0)
         (y
          0x14af2553877cef9fdb16c43873a475847ea44f79662c825b685ab2b60315eddcdf7eda49e354ed4494785c5dc5949d3)
         (infinity false)))
       (expected
        ((x
          0x16b21d9d87fec6a18f68b0c1a258c4529d1c3fd9a800000dfe402977eb26092484d5a9494997d4730f007c0bb805cbc)
         (y
          0xe68399063378ad4fa2c0cfac5e2ad11f4c31bccef5c0ec4af6cfb792c2c85db8a30004d6aff618619f842e9e4ee866)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0x9a3fb8454a3b566e82c77e9ef6af2500a701100831a166f115700121c9c1fbd7f7f692dbc42527dc4245007dc1db63)
       (z_squared
        0x10bfce4f4e854a07457a3c8de03cc37efa8cdb52ad64e0d4248678a40783b2a0d7a7754d3a08a62f8196ef11f54f73e))
      ((input
        ((x
          0xca30b2a18e5883e913f2fbb3b6357d8f0ef2b08735b7b9ebc3e47616bf603eb162facef3a3e038e41a4d7961b716c)
         (y
          0x818e9c403d76832532e6bc43d25262f223929e915b1fd9a01165a840036716058d882667d2efb1a613c325e1f0fbec)
         (z
          0xe5fac2916b08b78b528f6ce967bbc16bb04f8a7608280c9ad58a5fbc7166ba49045773a803c64ffdf38eef08362a2)))
       (obtained.affine_point
        ((x
          0xffb34b547f96136e832cb0634862f41b216294f5cfc3cfc0140f0a381e7dca320d52e63c7ec5dfd87313b6e4d7f13c)
         (y
          0x15c04f840f33aa347ed88df750d118f70230c555585af4318a362cb9b5affaac517dc41d861ffcd6198806437004c47)
         (infinity false)))
       (expected
        ((x
          0xa404dad58bdb25a89f215b2b1773401e560a6b896cd274275eae421798d26c8f9195dd8467aee878cdc9e0c8b9d31)
         (y
          0xb2d511c541663f60e5484730a85fe8d80cc47fcfa0674bea81ca450b78c6c1873d95d55d794ae64418ced07757f190)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0x1280cc881488aea02f38b20ef5f0e04a617dc612c2dc5267b5a2cf4e0875759ead4fe54c0cc9d59a488b5cb951130c0)
       (z_squared
        0xc1e5faf0a5c0eba12e92c09e144351d1820ac904d7cc6e9b75a9d883d173190a0baf94704999fe306ee6761032b10b))
      ((input
        ((x
          0x8ca6eaa8a5225fd3b319989d0997022686ddc5d2a1d885a1910d162a5124320111815a4ea055e305e0e1e5a9b3357)
         (y
          0x179e33a8891ba5c81f0db4fbc45123d3395e701cc11817d067fa61ec3d46e7f48fe36e86970e550572499069bed9ebd)
         (z
          0x13204f3082936a59a7318986c9f3a852ace35c510f2d5d18d4ec64d0146df6008e3b81093acd40b47d925d9560f2263)))
       (obtained.affine_point
        ((x
          0x10cfd4ec4628858d8b7bd232f6b3d0dc3863ff4a4ab7f621643e450bdc3bea619e79874915afffeea8e681de30eb8d5)
         (y
          0x16a1e133504696930573d982a1dbaf473b50101965376ef615d673a2783b2d959952f150844dd0d3cb7236db2d589b7)
         (infinity false)))
       (expected
        ((x
          0x15be5b28a0ffea6c130e3e73f5a8f658ab5df44b9e23263dd8bedf6bb7f770fa06d1ff91d524d77fdf68f39d0ee27cd)
         (y
          0x142bcb773009ea48fed34c822421f7ec03f4c4cf922d8e4d809357387bcbbf0b6579ad512d8262da2500f7e8990767)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0xc5b71154efdbf4a50fc6ca976c4eea2e69636a8fe95d202bc8b06b8ab614d20b73851ef8547def9577a9c6e3337ea)
       (z_squared
        0xc918533c0853b5d016b52aea892d488fdf9fd2699854aed4cf1de075af37f1c225368d65bc1f7bf72a8e1fa276113d))
      ((input
        ((x
          0x13d40441a43bb0b629999151349633f300384443bfbb5375cc098c40b2040668cce48b6d5867cfade64a75228ab1232)
         (y
          0x1658cdc6d0cbb86947af0e58a30973ed186a8245b35f7ddd9cc96e329edccceebc246193ea17963de6aa18f3cbad089)
         (z
          0xf0bcf83e558bba3ba62dcb369c919818ef06bb290278c9b0d0d9dcd16ddd761bc0d82819729392c8a50d3eba7670fd)))
       (obtained.affine_point
        ((x
          0xd682f08bd7ce7fe6d3949fdb81a3ad207a8669fce8c9112f4b3eb10163cb4adcdfd9a3fc4e1ec33021120b970b688b)
         (y
          0xa7652a489783d9537b84ce75b44f2014117fb800624c9bc98b9b4012e04f6be15875dd11e62a3c649aca87ece4bb21)
         (infinity false)))
       (expected
        ((x
          0xb7341d574bc6e5914425729485c4d6769b8430cd1d7a32e552434047173e0f1fb8feb7fbe9f70fb73865183707d7e5)
         (y
          0x1ade4dd48dd9c9f0db4cb14914baf178c76cf3743af571098f9f925ac199af49057e1b4eb635cf5680ff3e2236423ad)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0xce213b49c34084c128bc318b341378aa4899bda67a5c89741fa99c41e853c4122b002c4f9082e2eea61e92592bb488)
       (z_squared
        0xbf5861ad32cdc05e5f7777f451573a467150d9c20e3943b268a28343bc342b2114a69c71267d68cd69cbbc04439793))
      ((input
        ((x
          0x193eed3a8e884235b82839946a55c9f14876c384703172a8a840f45baf988881830c152057d6e5b2933444a989c4512)
         (y
          0x9a89a241cfadbe94cfa479787986771c2cfc325eb268678ff0139abc98a465e5cec15e8f7f247c7d03edcf04eeb414)
         (z
          0x1fffe5459954622c0f3c9225d6446644c4a7b28bc3bee3033245b85c5a2b1c98920a7d1c556ca3b66c04396e987f5d)))
       (obtained.affine_point
        ((x
          0x1596c9395110364af0cf11a49a3a03bb7f1a53f3597e2468a3b1e3ba90478a0f7826749bbcb771aeb989cd9aba90ae0)
         (y
          0x4175b7d32278e52f9ab43017a28d547e729d8dbb51d2c99a4f8ed4faae3ddeca7e7bcdea76f0f496a4699ced186d60)
         (infinity false)))
       (expected
        ((x
          0x1202c0643ccd5021dbdfcd8e875a073f94f51e4892fee28404ae7ff64a1dbaffff87a8e6693da640bb65ec4d1283266)
         (y
          0x15b648e18ab27646be8f28f9973c177283a6ed9e42c563fb52aa912e152c1862f3a4d7076e0a2ffa3db13d13329b01d)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0x4db3c5613c6ef309e717b0a3e499348290ac4d4a6712ca328001505033928f031df3a352f23e1edd347d18215cff1d)
       (z_squared
        0x50263ac605d32a02b06a2e0bacd154c22722a198efb5e80ddbe371162459eb75904bebcb715f88fb84c63712f92ac2))
      ((input
        ((x
          0x1262ec32cceb8b827b33fdf38f24caa01a0fe92a10af14b3691fc52953f3387789e82d9e758e4045df317d7b08ebec)
         (y
          0xcd183b6c1bb90997c9814e5716ad0d366d7d64f01add8e9a0fa5a1c3a2668192cae68550504a1d0e1f16e9dddecd88)
         (z
          0x140dfdf91a3df9e6b58c7fe062c16e13e1beb299ff936ce15cc753e4f30e885e05cc3c5f0e796bca95a02606f041dd2)))
       (obtained.affine_point
        ((x
          0xacf34d1a067a96b6c3128ca76b2dae01e16581613f8324654ea3f3388eb0fdfd1c0ca92cd79dc943de8e0fa2d89bc)
         (y
          0x2e98623c3e4f245fbaa50bb4f8164328dba1c0b2c1ca28168a6a870d4f7349b85e5d736dff241d659b20e8e7944999)
         (infinity false)))
       (expected
        ((x
          0x11084066a2ca1b84219bf3ab65d2ed9cb5121423e146cc7c341af80696b10df9201f0d75cdb07194f3d3109156e2afc)
         (y
          0x1035c971abea9aee23853d847f61a486eabcb9d4b8b04924355ac34ef2fdd4f01a8c1df5c2eafc178f1a4456613515f)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0x1355c8aee158bff8d18c4f0bcfd8f72796068f9d9f45d77b6d1138ca4acfbd721dbb9a126f5069622e40c83732b72db)
       (z_squared
        0x2199b8258d91fdcab099862b19ec7abb0496e9881882e377a9f1345f1fd81027cbb416280a35486a1b0cd4f96253f))
      ((input
        ((x
          0xa736023e3e3ec94873ae43268c0ca37710c3d0f662b1b32592e78c7e9d8727ef05ecc2e7a2a45b5c654fb6b3a2977)
         (y
          0xca03b65bf23a20aa5531ef1cb49e93df06646256335e227f49499033a36a06a4742a8ddf1ab4d46cb7120449767cd2)
         (z
          0xa3823db37b93496ce5c37a192f5dac4907a72b0de473a045553fdd08634b83529d01dad4e976f94375c6f694973443)))
       (obtained.affine_point
        ((x
          0x14b3fcc4a69e4c5477dbb4c79ed9ab08a1888c7d6fa9590f98f9a04911808c149aed13b9033502259c1d1519cfdf26)
         (y
          0x89e632316669ae5a059bd422a19b5e9a366319e2960ea89b6e1ee350477f892458169e96b001fc64a22a5e2123b90a)
         (infinity false)))
       (expected
        ((x
          0x1370ba7f843acf38911e1d62174a9d64f0e3fb2bfeeeaad627890382c0c35dbd79832f68e2e4aac23b48cc9958aa570)
         (y
          0x8ea660d71bf6aed753bcef96beb7632b1642363410ef733f70f308f1ae3f5dbffdfbee8945ddf8c11a4ce01a53c836)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0x1adf2a43eee5618107830f6a3b3346d613b8dc00c1c4ee465b43b46d5d7afa79472f12b440d0d9d4cffceac3252c44c)
       (z_squared
        0x1ad1b40f6b73241b9d539186e42bb59debaf08d899907d8c9f0bc761d48204a15510c418a1933ded6c53084f35ccdd6))
      ((input
        ((x
          0xb1ac3d53f22ebc50c2f49675031a52aa83c0198efcae6aa5877c9755fa503cacc813dbeec50ee6221e7e7444e7ab44)
         (y
          0x11110fd3e314ff857846b67e4aaf4217ac0048508a13b0100e189579315ceab9a0da11003e092424be2f402f44f49b9)
         (z
          0x16fe6ae9531b8982a709e1d5ce7c9bcf2b7f689775d8e161df75643cc3e3ccc46c0a262e15822f8985521861bff4172)))
       (obtained.affine_point
        ((x
          0x882b26b428e69d021f2edd714ae4fd175c9c6db5a908cadc6d41ab093068d153669d4ffdde64ffe04a5382f5170c8b)
         (y
          0x4c22b5660b8533af80a3515b538bc45023a9dd8ea4448e6f3e9603e30daf9e2d9da7dc506b14300b8f3f25b9c6be26)
         (infinity false)))
       (expected
        ((x
          0x13fdb2505f39fc2a8da681195f0fcd30e21827c5ad3ff6c6eabfc920a2aadce7c95b0397f3223a35e8fd80856d8b311)
         (y
          0x1a31578ea2ce9eb9b1b78f033de5a62fe29bc0c3424fafa9e793404d1057ac1236cb41badb1b4a4c09dd61564eae146)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0xd0db76598545055538973b833b1d8d60c14d9be83d2899350d8726eea8e1b7038b1f3873ec9c987947428c128ef152)
       (z_squared
        0xb0d9e9e647815fad780d3af9b95a6e175f1e773dcc639be2e2ba2fff88e3bd86092c29a38356f5f6eef17ddbdbc7c4))
      ((input
        ((x
          0x7d5577815afa5f70909a669a72feccf5cd500280bb5a849e6bf91f57808c9839d66d379af38a4958cfc98504c5600f)
         (y
          0x8cd802aa319d4535917755217ed85ef49413a9340307e0301f0813e3696a246957bdb1ed2552d7b4d00a4f98099615)
         (z
          0x1a1f56a56f1159c4969b3001e12f4d36f5f970b53aab119360db37dc29f2e2206d119193ade633154d71be325a48cb0)))
       (obtained.affine_point
        ((x
          0x5936f6ef8635937c188a0ea63b6dae1a95941a6002d01087451426002d55fa65531c2da8804b05332385847295cbdc)
         (y
          0x4637725f24698cb1e2495ece01fb82f12745b74dd4550ae5f44cf20993d0af678e4a5a4c9e928ea4157dc228c4a1dd)
         (infinity false)))
       (expected
        ((x
          0xdfcafc5d908642de0a380e879638a25b411c863d4f1b8527612216240b12f3d99f8d974f394c930860958953fe4bfe)
         (y
          0x12aca709fc6f8b95a682223d1cd3da8ba8c848e57329c3236be6648e2743338b8b9c07688fa5c5adfb9bc5b50ab75ef)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0xa9507d2f02a3b2cd5f49588d8807d424eea8482bceeca21b0d7488adc87fff1d9b574c4553b8313004e5712ad5b737)
       (z_squared
        0xf3dd55f0d8d6e55e8c70d3d29fa655898b6e3f11beba4e922b2e85e7e2cc999c4eaba4077423ad8fbf860507eef451))
      ((input
        ((x
          0x642f138c8a10ba1e6d6308f6805a9013c160f8000f2c20568b51659b1a6e031feef6f0e2bebbcb36389f023463a5d9)
         (y
          0x72aefc580c3833adad15055f035f5a568eaaaa54fe8f60d57d6129c9362854136da59d544620511226fd0949b9af73)
         (z
          0x12b5c88aac4deaf53ddb79835ceb9865b21bd928d4b74335c106cbb1c3d67bfedc5bff04b39433e3459ea359bb3e0be)))
       (obtained.affine_point
        ((x
          0x11c7d61e9e00b0187226333180a2570db66159f353815a73288cd16a417cc1688221ef0c094da16e2034e775eb9debb)
         (y
          0x15374efe2777a6e40f08a81a000fc818be416033c598c90b528f37b9b54461c406cfdd35baeee537a6466caddef98e7)
         (infinity false)))
       (expected
        ((x
          0x78ad6186cbea290c11252e2db2961324586e61cea97c66392b9470a6d3d8ecb15a8be87b892dccd3f22b10104fb5cd)
         (y
          0x465772aa52034efc3d11142940d99d6d93f6b020bcb499a98f1106f212be3ce8ef46236afbcf848d3e8b03f030988d)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0x15f688232d364e7668e2fe6f05bba7508ff4898574f8dcd0f76d6f51ebbfd5ec67b8038428e3211fd5e896d3318326d)
       (z_squared
        0x18bf7f906a9d06fcf7afe2e5e15ab275340a4b29e0e188c1940210bfabd75ce5c8a3de54b6747cabdd30bf07279ba05))
      ((input
        ((x
          0xc2105f5b61874c23905e753f2f251dc5dfb2617179f27e1a64cfeb8f8fca6db24e0bf6ec61125d5a2403c2fa1705cc)
         (y
          0xbd2e23167fbc5a7aac34a639376f51e61634cef2f0e50e28573f767911d6bf996c293710f9f47d59bdadceeb75f9f0)
         (z
          0x166772060e021e84233c15ebdb21d7ceac897561d85aa7ee56339b6d7220dede2340f6bf01c92cc4456ead3e0118c58)))
       (obtained.affine_point
        ((x
          0x7bc48a78e63f8c23dc72eafe64dd5f1529a8b8b312768d30d3329ab076ca3c47489d76e84a850846a41749e09dbdbb)
         (y
          0x15f28e08923c621d887b5047d8a5a4d193084dc9cc8378510a7804ecb44e04f77de4eeb8138e5d5191e52143b0dfa1d)
         (infinity false)))
       (expected
        ((x
          0x141e104fd9a838c7a8df0a741d26544dd19b7be1078d09aac89641bc15c5c189b8b3777781218bf00baa1cd99cc8525)
         (y
          0x60ec37eb6adb479f3aba6b579aff67881444306c20b55a683e79236539e993ea6247af953442954fa944dfa8895c1b)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0xef81b1fd944a21c256955cdbc9575ef3e7967dbd948f7468404e8ac9d85c6b36ce5b4355309eb7a1fc63a35921a444)
       (z_squared
        0x101e90a6bd44a8a1749368353cb6ac1ad696e082902130dfbc3b082a3de0681a9ffbfc28d439e408c4ee06e22e50c1d))
      ((input
        ((x
          0xc0a3c37df67570c7ad14505531912c7226736e49ccce7e2bc20184057f8ac30246b76504006cb1e97d56734b2fdcfa)
         (y
          0x11267c43f613d52ee268ae3c5ac4c92dc8d00542083486ab0942596a496e75d040c3dde68f4c7ec521793ec9a64ed67)
         (z
          0x19c46f53ec4890afb8a3a5d6fbe1b31b61e2ea32114b8a8f3b323da35e68eb15e832dac79acb9a1e6f2719d30f46239)))
       (obtained.affine_point
        ((x
          0xbd217f9d515b76eacdaaf340dd07253fe6edf6d46c15267173f7a0d2dd70815f027d68c44884a4f27b8bc4a1fffd1f)
         (y
          0x15cf00d96a890d7b85b90ddfe7a9363a0173650ae042ca4329eedf69a208bbfa944fece83bf80d774c2aec75962a276)
         (infinity false)))
       (expected
        ((x
          0x44379649cf132c9b4d25cd063619b8e42d8a4df85d6c5a2cc85399a322655007aff6f9b4b04cef3b60302acd20e8bc)
         (y
          0xe05e6fa84e71f6c862270f746ec8fd60aa57131ad51645ed308ce53e1463969f27b1135d895cffd6b05bc17465f4cf)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0x144d50cf2f68992ce8b72baa314eee23e46986cb4c5106a846d1d9473bfd423b723dc62bb1a3fed5f44d6f93233cff2)
       (z_squared
        0x8404851c55f1e97467fed2d8bd3fb094fb65a373bff9f98f7284e1402b830481d1405a987d8782306218b7d7318939))
      ((input
        ((x
          0x145655cc1f9b4c5c77221406ddde68fef3bb24405a9b34a00ca71ee7715907fac2ac504d4a5f31f0213f5cd80603f4e)
         (y
          0xa65f189b8e17235bcf9987e9931996a3a4cbc2c4f2d859e1e323bda94f279c6d5cf816cc7bd0a1b78d653483cda887)
         (z
          0x1ae3a4617c510eac63b05c06ca1493b1a22d9f300f5138f1ef3622fba094800170b5d44300000008508c00000000000)))
       (obtained.affine_point
        ((x
          0x788a13e24d6954df42abfd47fa869e21368d7053f55409277a89c4311cc608ca4c37d3ac7a2930e16f8f0253fdf0d2)
         (y
          0xbfc82ab6988641806d7ba5b3a173d3eb3efc2024b98c6aee37a49d30bae60a15380c7c1baa8cde3888bc0505524d7)
         (infinity false)))
       (expected
        ((x
          0x15169cdabad4eedec51921070046c03578084875368a7387af2fe1e8f39c8bc49fbf85c948e4ac036c22eccd4eb6d0a)
         (y
          0x90cb7e36486581dabdad5eaba3f71fdbd6fcbbeb9ebe296d53efc4dba6b2c0e6ebf0e5c1b29d6cbeb27e914181ec87)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0x170f68f4063774550babf6763443d6318443655ee53ed84bf61450cf0692482bb90f6f56fde4fb94b74288d14fc2fae)
       (z_squared
        0xd1181c48f63c56edc3d120b8d7684e2c1bb627938d12b6483c62d1638223a45081d8eea0112537e2d447b915bb08d1))
      ((input
        ((x
          0x15ff2517c695ddc51db4aeeb7856ff7ebc3ec553be068ec0e72980d2995cc9ec0fad11127440c3c68c92dcce048f6ca)
         (y
          0x17b323b38538cd8d2fd0f513efcefa80643a8b36f8b18d0043796dc4c0be79cc5f871769554b55e6a0fd71aaa922f94)
         (z
          0x13b028d2e7241fe165c10c3f58d6deb51931da8363f6e1742b7c0bf033ec7e48ff002706603c3f73d4ce5385ce2bd01)))
       (obtained.affine_point
        ((x
          0x115e74b64812f0a01da507178f144ba711e8f95c5b75fbac5a80ef36784bdf9c72f5f7503b7bf5c0e2cf2a91c736fa0)
         (y
          0xfbb55af0cf63f9bd885b663aaa4f6e576a78060a3704827c060c1c1a660404c5cdf69db9a4dbb10b3fe283f736059b)
         (infinity false)))
       (expected
        ((x
          0x1038bdc43d8e4a7064e33698a91873808af5b53b6cba5227b5ada8ab7916d57ecb7c2d78f62acaee15e76cc829a8379)
         (y
          0x1659c11f9b29a48d7912d0bc26c3656aa3482036804554139e462654afd3da5752d809be05ae22578ca3695cc619a07)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0x12eae8b8c4794435202bdf4371379c24cc86c91c1f70121eceb4947ae8bd4cce10636a742ea91c27036abe8fdb75806)
       (z_squared
        0xd01eb9a086e56effab3a92d9fe190b48f433e71d6c8ab0914f98f5444d8f3bb32b8afbcaab9ce7b7f153ed9c28a82))
      ((input
        ((x
          0x5f79064e4d799e934eaeee17e5dd4abcbfd1acdd8bd955563f2d2b66d6bcb9f096560fbdc6baed8342066c62364bb6)
         (y
          0x12882a1293c45a1f409a32c7ba8521b9eafc20b5bcb8980e05cb206f8a7e6e7d1158d4da404c0b6a7287f810b78b951)
         (z
          0x4c2b07b5df387cbb6309b194a5e21486a2f777c488b5c5a18e6d2ae89e85651490d161a6dd010ab20a225fa9b387f5)))
       (obtained.affine_point
        ((x
          0x17abf6e73deaeb4ffc83984fb2957cccac7af2210494599df4bd2aea1e476ba5c6ed906aebb594369305082d8456330)
         (y
          0x4e7d524b6971a1fb53cc10018c8905beff6911fdc3a406ddf35a47d89500423d2c04fa3abd5739bd0bb55c3fbc082a)
         (infinity false)))
       (expected
        ((x
          0x145490cd47d4062ddc790aa4466f7901b6381d06f4521ee0d9b1498c5881a8e1421fa0507ba9cf18adcc207386c5262)
         (y
          0x718b2f5f7980824667f786d9685a885ba434786ff26b21987d01a8dd9c2b580966fc72286290a0ed9b9a77ddcb3eb)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0x1997c3e7f032384fd131ea815b565fc99229a1478361332f9d15ccb2e37ce00ede31768a48b3b2f328e25a072074428)
       (z_squared
        0xddb09183eaa8a564619f4532536d445bc918366a33b82f3aff49c0c1badb9eef00a23d0e0a75eb1d4d80694d195db7))
      ((input
        ((x
          0x16333d04538d8ad0f912015c1c14b8d03e96a6ba956f1a78f642f4aa4ae2b65fa7bc576553284b99094392529cefcfd)
         (y
          0x27ad15aad99d5043ffefb298b2f383f775ef230c60629ffe2b87c729155d67f9f150028f91f8f255b6b7d681d7b785)
         (z
          0x19789ddaa7545eb09065302078ce6859350167c5f7bfad8fec5f06daa94d2ff2d541ae26a88c4e73a35a8bc71045e4f)))
       (obtained.affine_point
        ((x
          0x8f899a03b86014f2a5c5df120da96d5330590735f6144723db04da2b210b9b74e702cf6184be562906fab144ff3f97)
         (y
          0x1df233dd74aacb820f7fa20bbf6d5b910b268780837edf00e6f362390583bc63268d670330489048c11fccb9f6f8e7)
         (infinity false)))
       (expected
        ((x
          0x1144d486afcaa024d579870a43adbce9f7ca7c0eca59dd233fea0448193da611cd090716f8b67f0e199a2e059d7f17a)
         (y
          0xf48b9ae5de086049e7097268d7e9301798e36b327a246fc0f288ea9cf7875855c36e9f78dfc6bae8c70bcc9ffbd9e5)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0x121a5ab974cfaa378558abe27c3a39dd91347423ae23871ca1f9c165cc8eaf04feb531ae1df3150eecc5cdb26adf1b8)
       (z_squared
        0x10fbe831debbd3d5fbb743bc15ed8804d37906bbd2bfbeda906edc89931174031e4679ef4c7d709b936d9928e18062e))
      ((input
        ((x
          0x682e7153590f6cc73df5621f04572b774c55dec3bda196419f658183db8d87226d6f0fe649ebdaa73218f6a1b9836a)
         (y
          0x6ac2dfc9a6eee00949cf572bf17d381164746aaa2b168c01e18d6b55efa9ac3408a31651f02f1e096f4d3a430e59d6)
         (z
          0x1ae3a4617c510eac63b05c06ca1493b1a22d9f300f5138f1ef3622fba094800170b5d44300000008508c00000000000)))
       (obtained.affine_point
        ((x
          0x3f73b5ff87c91f1cb691fee3f21278cd407992944344e8d31d1fcc9ebf67b887194e0ced1191a01457a4cf807062f8)
         (y
          0x16bb2e08aebcdb1e47406eddbaddb35e07d9e18ef17a1921ea42a7be3df66218dd2d7f1c7d41a682bf3e66c78620cd2)
         (infinity false)))
       (expected
        ((x
          0xdfeda1505ab917d61cb2eef6026c63bd310e63685cff18541989448573fc4bb7f17105b5edbbeb816f758edc01245e)
         (y
          0x8ca309624c2dea45dc0f5cf65dfee10cf98525a4fd67b75172f1356d0254256d62c20f501a092e6e5f94fe5de3eb87)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0x82d6fbcde0c1f436486db9522e133829c8343b61aeef7edec781ef15e29f626c8bd84e1ef4ef6ab40f18d73d6a4df9)
       (z_squared
        0x154bff6a9afdfff79e01fdbf26552794eaec88cfe530edb0838bd3636115f4d837d7435a30606d5139fc2e5290c2b5f))
      ((input
        ((x
          0x101437344b53274ac62ba8464c4030bf55a6340565fa636e88f4623831b7b0628de3471e0fca32fa0e0230c1d76bc78)
         (y
          0x2bc08fb7e446f3905bc04bc440996803e6dab6fb44a32ca5b58d27bb6a22660b751ee1e0192f7551dd21baf8006e3)
         (z
          0x2759a69587f89e933d5656160063f1aeb5b2832e2846133a4917a281053a164f9e2a520e0d059f600e53420b0bdd97)))
       (obtained.affine_point
        ((x
          0x1f3536c6517c5c7dc573da0f9fe64cf9155a77700041164a147761d570ce74774aacc17cfa70e8b162b3da60bbffc3)
         (y
          0x2226cdfc44d46f0aed7066dd597888817d7cf17909fcb2849abfce2121145265518fa427ff3f3f55e60d10307f460)
         (infinity false)))
       (expected
        ((x
          0x16736106c936ef708e114255e718ee67598bce1b649748bc3115ef3a5b1e26d54d34f8b770888c49372a6b137c05029)
         (y
          0x1830398df71c274ae7c26338bd47d4705ccc569c9b42f499290602f80ca98e8cfb6491dd521ba3924326a333fe0257)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0x8aff9e21a6989d3727a7fbc1b454ad8c6946ebbb95727f75bfab188def595ebd9fb7e203a45ee370e9790338115f7b)
       (z_squared
        0x6c3c3e09a194c4f18d8462c7a8a24fe687bc7f65d6749ccb888feac2b0a3e8ef4df2cd925561a075805f496bbbb12a))
      ((input
        ((x
          0x149a31fd35047ac1b8dfd2d7f2aa014bdd71f4f6053e08e91f14c58baace4499f8876d70bc8cf05c4a4f6b2d7607e9f)
         (y
          0x14cade774a95f2f3ff448d894dc08899397ac3045403c83717e64fecc6300046e7dce9369958acdb66a8e6eba0efc7e)
         (z
          0x12ce9e6c7b3c2170005444f6a19b6c71ebb42cdba9af598ffd7292e405fc2f09ac4fa680f920fdd657d5d769a9263c0)))
       (obtained.affine_point
        ((x
          0x194812e84b9bbf39a808b17532419edda405d3dce0578ad8eb089ae2b5f9dd76d40692148d944ff2a85efc5ff5e8235)
         (y
          0x13af950abb2c69b64db95c0f3204ba704f9340a285736758695413160abe5caff00311b25416fec07b95a89de2a199f)
         (infinity false)))
       (expected
        ((x
          0xb013922bc0cfe17c3e22d39d7db10ef6ccc0a4c567a02677dba6d7c42dfc0a286266dbf74e801cb1bc138cd709e278)
         (y
          0x40ef3c17ac04f64d75beeb69cf5920618f50af442dd22b1fbb9d76d89377d65482584ce6076f88e4bf44b0666b5d7e)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0xbd9cba96fd59e4161a11557ed07e703511e45f1136791f9514efd7bb05920870eb2a5e64a489a3a1e7b9a637614448)
       (z_squared
        0x18b927570602778878e9a06eaef53a5651f97eefe01aeafb59608e52d0948f40992e177468f372ab3977c688316f477))
      ((input
        ((x
          0x8f822001db45999d9fc9b1caea03f38fd673586ed5ed32f6fe9fd221fc9fcfa5a0b88db44d59861afa11761c5bd2dc)
         (y
          0x1e40773086d7b40bcfe31b328edc1b99cd60596729408bab9732e2671b39532c358e03e84e55653a10032505491ee2)
         (z
          0xcf00a7d0c7bd8432a3f24abac27d374575271ecd91fe2c6f2c8f7969dfca0e0a6cf0574b90229bcfe575eb9f4f3967)))
       (obtained.affine_point
        ((x
          0x7c9a07e336fe81a19061fd0cba07ce150be70f61e5b15a4e227387e6f5b07f00c88352befc882ff52035a8c8700f7d)
         (y
          0xded0e7722cc42cb8d9c50cf385ebb82410bb0990911536eac18ebe8b2f14a4456ffaf46731567d3f339bf31fa1f646)
         (infinity false)))
       (expected
        ((x
          0xae2f77206e2cc3d3f987addb1eb61d60b43508b17a2942f2133c9c9a168a1664b8c5de5e6d80ae3b0dffb4329d5778)
         (y
          0x12242759bf476c875d791a1054440f25b83ca805a7cc24010d86d2590a70d9b4b65f49924182cf57aec932670bc7081)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0x15d07b8c136b4a12f827d8be71bcde17da2892e8d139ecc1b92ef00e5e269c602ee6bd1ccf80a37073828304db9fdb3)
       (z_squared
        0x6ee3fccfc4bf12009e48762a9e2dfc1d08b4d67b257e27e5d34884662cb7671d21058e247b52a958bc2ca7635e8411))
      ((input
        ((x
          0x1884a3edd8e3e2134919d177ccf5c40f89cb2bb235af07b40bb6009ff04bd67a983c4e07d57870b19931d52fb133daa)
         (y
          0xa72fcda0416698524df4aeed4b75989ef2760742860eb3558266c69f990d5e4a09762c0f5bdad4ef150e804157093)
         (z 0x1)))
       (obtained.affine_point
        ((x
          0x100c4546ab74c95d3da9d1dd772fcc8b0bf87a22e39d38ce251c6525bcfd7138fbdf071e4b5d5fae3e62cbcdcb18e29)
         (y
          0x9d9f84c77b7ecaa826979fd6e8f2afe0a5f9daacdb235229dac9f2b2c0994840fbfd0803aa1ecb0ddbee716f2043b4)
         (infinity false)))
       (expected
        ((x
          0x481719780fea855cae82c4a0a133ab1eb86515f10f6a231d0ac61fe44da256e5242d52ef2cfddf910605389c5df68)
         (y
          0x5c299e1a5c37dc0c851ff63f7534e8a7fb83ae7ed55fde30c4c108c3bb02d616490be81a03f983cc3f10d315cab7ea)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0x19524bcd16a43b78f38c09f75c8ab7efd5d1f2325414306b38aa9cd08d19bb1759b640d61470f0bc905ef24bd7eb75b)
       (z_squared
        0x1ab502f10ec13c68cd78e5379cb0fde96758afc92b761c8a5ecceb63ec22a66b6fcff79ccfda038d1cc1dc8a82aa0c7))
      ((input
        ((x
          0xf33203500a4b534ca21ca97b9234b7f2c7fb1ab4d0d90f40c0e622e451fe41722b57efff4c32bad404f42472ffcd44)
         (y
          0x15794fd7ae42a0fcc8aec1f22290e4991ff20e78381689687d10e9ed9a98fe9775afbc5fe861619c3668217f2d6dd58)
         (z 0x1)))
       (obtained.affine_point
        ((x
          0x994b45741557ffa2242fa771270d000beb01a27377bdecdc70f904b3a4e009d80a45cb06c6589a7c4838e39905722)
         (y
          0x1143529c18f43270985e422e32cc3c7fb4da7874980920079e05d264a88fd0204ce66e65e1d0d856c9ce136bdcbf075)
         (infinity false)))
       (expected
        ((x
          0x9c78a4a396bbc6c949efdb8d13774ad5909378711af6926ff58c6a8406b4849d78452dc3700339f2335f4e367055fc)
         (y
          0x679e44d3a71a00e2e73c71def0386d40633e90a7d653914213d56b04d7c3ca33251458538c01461b7341d7687f9278)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0xbf36d4aa9f240e6d4fac3d6166b0f2eb3d7d77fc82656775cc0b49fb8cba04e9576369f8132e5265745842bf200e8b)
       (z_squared
        0x44c8ad135372b9a88e8c4c3ee65ffbd7afc289b301241f426f43ef0e3fbc2c519102c29189c99bca9c8e57b5c50b75))
      ((input
        ((x
          0xd8e125ce1064116bb399de7fdb4fd1acdd3c2cd21b2a3ed8dcab0fe63d7489ef985b08123a4567fe706659c8c848f6)
         (y
          0xb03e917cf4f7931ed1a1d38f44c9101edb6c98d6949db252990159c384cc8af9d1ff223bea31bc3219c16ce5ab175c)
         (z
          0x2b2bfe277923f685776193068ce884ce52eec529486ac6ea5a6741530c5e050332898a5373cd02735924b8dd974b3)))
       (obtained.affine_point
        ((x
          0x16e52317fa8bbebd2d1070d65f813db399254706442a6690abfefc7c3f75cde65a25ee082b36fd98d2266f5b57b6031)
         (y
          0xa8aec14d00e47c5ceecf5573d2f8c253ad0ea0d7fbe64da32bd5beebe02ab62713ec3e7ff2c08088725e8a38bd2457)
         (infinity false)))
       (expected
        ((x
          0x174374e6e958f69d7397886b6eb5fe56e9496191c29e75c3fe3c97bb4ce4d06027812083f4a208bb8657168c98312e8)
         (y
          0x17cbe33c495a2f150b34d38ab907bd6eef66467f21164ae907ba3f45a4387911c07953826fd23894cb574b3838ed57c)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0x75c69fcf99ee315d1e67b4fac74cc5cdc689171d7a3ffde2e76cb4c1b5cb90e672d61890a0f7089f087b817eb53483)
       (z_squared
        0x1a6cbc7f8b676f46af35e94eda14e1494b5f7d577b7d70379eb2073f2e05e06e9099f4b5ca2ba6c73cc4d9d2f2f6bae))
      ((input
        ((x
          0x188050973baf928df5cb8e8e731494c9b5fc67f6e1b540d14c8e0e3411bfe6513fd24b3f28d3076bc0dbc0298df4226)
         (y
          0x146e035cf41c27b58b7f4986e8d8d4e27b57991faf32a93c592737566205f2816c742d9d7bbc0e2193be4dd87379d0d)
         (z 0x1)))
       (obtained.affine_point
        ((x
          0x3f8f9d81fa4c3c55f89ba951997aa599ff5869336a68e5782178ec964ba1f13274eed20a0050aac5ac0e0c3a721648)
         (y
          0x4656b328d36e5cd43927f5b05fadcba4a7b3685c80aaf1a962ddce6027acbcedbaad846da06e05e0e52a155146d265)
         (infinity false)))
       (expected
        ((x
          0xb493480c84941667993160de932f5407ea56dc8d9690ee9182114169aa6409d6a7d17c0a5c2647d02c6353ca72c340)
         (y
          0x9f8b95dd81aba55bfa825bd39d3492c8e74897e79e0174bed90db1182490b37b6621bfa434f6836c5e1f66fae68d99)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0xc6eaab7ec690982b46a52c4ecb7eedee66166916c59e0a152025dd7becf750e2055a5b9e8c4ed254efbb3e07a4995d)
       (z_squared
        0x7715bea202853cd5499dcca2b9dec094315701946d735d6818175c512d51451bd22648eda986c39520af124258445e))
      ((input
        ((x
          0x110923abb0bfae6ca630cc9e86fc936cd79f2e459ec0dfbcd9faeb22f4d78261397ecf55b55dd57cb4ec6370f80ec61)
         (y
          0x13c68093af0259a38fe3e03592515eb584cb91ead6f7da21befbba6a28a1975a0ec5a924efaedcd7127b27a364aaec4)
         (z
          0x8f32a036f79ba9e4f23865c3f1dc2d10f8fbf23e3256d8260bdae98e866aef77c7362eca9c949af44120a10903b72)))
       (obtained.affine_point
        ((x
          0x135bb5a8812f442eac3cf3907e7124905b3642285a03faee78b793f3b317ff267d532df4535cb8628cc9cf57aaece3e)
         (y
          0x10680ba4a4050b953776290b7f8d4df4f20497c5ccf660bc46e7ec3b944d14e197be410c62ced19d4d708744ec86179)
         (infinity false)))
       (expected
        ((x
          0xa6d8ae9ba9036b3049ce32f830be2ac4a16fba1e240e78f4bbac932b6d6606b8423f390388a798b30f5b28652a320c)
         (y
          0x551fa10dbc4041da5ff7822d3943488c030698bf431150e44595b849c49848bfc6dec86f5b75f2060b1314c535bcfc)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0x9b2eb963944e5b075049d69323b74975ca573542690f0cd6965cf61311106eab47db4ce20988b6e87b1a8e7d53f54)
       (z_squared
        0x1941d78289a7eaac8e5acd9de6013a7d96cf31de53b050b744fc8c6f3acb2083c124fa9be24f9ba95448235f71bc326))
      ((input
        ((x
          0x112fa4a76dcde26de5eff73f5441fd5dd57694938e78aaf842b7f50b1e62b9eae8361001fa9a28831202b5c4c6c43c8)
         (y
          0x1342424eb7871588f28db7cc18dc3418de091e04a6667fc63b0b0be68870838e02e55f0cb524318b8a1198ea0419f9b)
         (z
          0x8b158b52fd67282fcbca82f0f1c8e366dafccd5ee67f5cc7f66bf9d973682bd3f7be7418af749cbdd4f108b141b87d)))
       (obtained.affine_point
        ((x
          0xacefd558a98cfef84d19753bdbbd39e25476a43ed660bcbf64edc84b54ffeadb7e012f520e035a93592601411cd0bd)
         (y
          0x58d6da84c4748585c9321c7887155e255bd2bb1b2e7d315498c19c1caabe19950e8d0d8366369727ea8cc65bf59ca5)
         (infinity false)))
       (expected
        ((x
          0x1999f8b353e0ff6bc4e1c667cb457cd9d602479521c3cab03475aaec4463b6d02e15203d1fd065e553e88d4705f4adc)
         (y
          0x24711383bc2b021c88a65a5fc9aaf906bdd77741475cec21a43087902919bd3701b885cc1ec5ecbc5b40c808b68525)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0x8d368f8ce9628e3bd0eb2f89ad2eaf7880a21aa8acf1bb649590b1a3fb34eb8d57196bba691fa59261f468dd5b5210)
       (z_squared
        0x475bb5b45704957e181af099d59b792b023a73371bc53bc1813b9e67914b90a03194f8d9e10edfffdbc3a79cbad72a))
      ((input
        ((x
          0x171768ac7f08939687e0d6bca560437e0bd78df336b17af6f07a4ec7ede51fd6da5e64834dd50d430d1d15a0008c5f2)
         (y
          0x162c9889a25916600d2c5ddcd14ffa0cbef9ead4373410f3349aed23a043da480b53743dc114ab9ba3ea1f28d5e8109)
         (z
          0xb3f4c7d971fd10d9fc6d7c8ea8161beac78ac0aab459d3c6ac34c89070f5498e1bb5db5e670f8cd9ff7c62ee12fd4b)))
       (obtained.affine_point
        ((x
          0xfb2184ef3fd96366f5e89f14808b592b97ff54dde7eec2c4e0db7b448582a416fb498d264b5dbfea237cc5dca01abf)
         (y
          0x503b37d43f84dc967ba2fdcee5e35b87858115d5cbedeff2b250b86113bda0939b268e32e5cab3a553d88da7af20b8)
         (infinity false)))
       (expected
        ((x
          0x1a5bea19da8493f885d66669d18dcf072aef06924239935f533d4e68aff98930a00fb8b2e8fe4849f51e641971e2818)
         (y
          0x167f64334a063bf7fa2df1be90d77011b3be54d1c15886bf841a41d4ed7f7288c1aecea96af69798f3a1d92bccf860f)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0xddf023529924a04d62f92e5bc1f20500c63fd92193adfd246f285943d07085bb42602dee3136813c9319d7d614a97e)
       (z_squared
        0x853596dfe0195da99bae8f791bc6a9be72eed6603924e65c559da9f71fcaf441e5c5652367608b2811131328a56b38))
      ((input
        ((x
          0x127b345aa626b16c7564280f7fe5bb31bdc7f3bc72f0b87caf2ed68f23825a4aa980692c247c4fb121795cc7eb9f9e4)
         (y
          0x768be6418ee6514adda5481c1b1bff167a3b8e50e99fe0824eb738bbf997868a77933601111102cd6dfcc17a312b80)
         (z
          0x18b4aac1d786c29259fb34e79e9b6fd357d655c4c088bdabbc89d5d3ff234df8a1cb956d1d2985534da4d4e2b5fa7b3)))
       (obtained.affine_point
        ((x
          0x8ef2335fcbf26486b31bd944ecba8ef5d935eae0a63333bf32d7f2fd09a5a8fe5a8eab061aa7ec419e2ecc7fa2c4bd)
         (y
          0x160e83732d1a21299941babdc1dda73591d026aa0433fec7b649bc7bf9da870c8d734d88ad7a12a619164923d816e9e)
         (infinity false)))
       (expected
        ((x
          0xd0e6842fd67a170ff3f48667f059fa51f297fc73aca18ae70b1bb2b0d3e3c4e8e0c48fa358c4bc682901030c75ea62)
         (y
          0x9ce8dac14e34f8208fa04f094d43a3eeb7531bade17c2b68af592b2caaa1c8ff74c304141aaaf2a83d6e01204a483a)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0xb91249c48102ef36eeb9cd7f214638270e52647e0f40c8e3eb38b333ddd042bbee4867ca2d6915500a0e04f6fc267e)
       (z_squared
        0x19dc2efb754364ad07c298aed8076047211c83ec158d8a2d5e43997b7fdf133c4f5ff8b918e2d24a33e74d661415283))
      ((input
        ((x
          0xea47fd6473584d15491f69db699f1f0c7bf542e5919a69c11a72d381f8469e8567ab15b84f9f7e94dc42e71e12ba31)
         (y
          0x116566297fe33184dbd5a736c92f2a1bffd41ec3305ca528dabd339aa79caedbc2275b86bb3a1209cfe8c2937aa6805)
         (z
          0x3d648b76c3b9f386b386fcabf6ddb4ac4cc51c5b6e44b6ca7f29d45c46fb981b7463e64bf5b950dec739411bf1bad6)))
       (obtained.affine_point
        ((x
          0xaf06c64d7d5f7e61652f4aa99c57cb36ce0771d559e6ab8e39423c4cce9f611912a9f72a0dabeceb74c69196a09b73)
         (y
          0x3e243ab0f5256d2d5a1c9d3abea28bc59222e0d543a45e8a76b7d52836e4da1ec87dadd4602204c4e568d408dce154)
         (infinity false)))
       (expected
        ((x
          0x129b1d0812476f5e979cdc7515b2a4b51cd2ee7bed55f2053aba05306c53a818127159cf72567b74b7354268250c1f5)
         (y
          0x5397c12923b4ad8739f17223241d0c8c5f6e4e7f5e6f65658390dac21210d4f205b7390de4a19dbd0fd61b282849b7)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0x487bbcf6228c686c0d33d0fe0e2cd119b43281960ab3a09dc401a0421e6d2d96fd3796dbb25e1052edb89f079076b1)
       (z_squared
        0x19b63de30b8767bbab9357c8b7252ae80dcf149903d4538a0e2159d70cc8f032c1e25c845cce8678dede429cf8b676f))
      ((input
        ((x
          0xc13c22bbecd110f942994c2cbcd2e0944786ce61d6fc1d4c6746d8c7b903005a57b6ec6fec4a2ee2fdb4737cede6eb)
         (y
          0xb77f131f2788d5077acf68497c7e3170f2af43cc48d1ce3323010cdd7a315fbdbeb74febc39638908b3562f57df8e2)
         (z
          0xb576a75ea4951d46a209db9ad06efe6fa9c8588aa95fdeb807a36d6ff7bfce278751ae353bb80d5e4b3415ab514bdb)))
       (obtained.affine_point
        ((x
          0xf3f126dab6dc7a8bcba5edafd36a0b95995913b36fa941ba16348c548d9e6db60d3805b29cf5b5d136256b2c0ac8bd)
         (y
          0x19f52aaeaa953fd9fda9b49ca35f1005e5542a1d142c14b5adac7fcc961350166e384e72557d60479eafbb37a4ebbab)
         (infinity false)))
       (expected
        ((x
          0xd7e02a58aadcb702e07fe2a21ad3fe432b900e306c00fb501e7da40f232000ce24db995773675d211e3dec124f7923)
         (y
          0x155d792ad29bf2815a520830fae075b3ee7c426be94b5d24280973c783f0fcdb02fd72414acc1592dc8d7a8f4a331aa)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0xb108ffa3a8c4b25997d71bb6bab6e769dd25b2d474d61e03c04e9234f717d1be12aa03a66b5525a7e028d43db60702)
       (z_squared
        0xe888a0c21c0eb1e02debdb8c2eac2c2ac737693d7fd23bc5c889775ce2287f8969b1c87e2b006dced2fba7cdcf9292))
      ((input
        ((x
          0x1058a3dd3b5ebbf4fddc24edc37a5126f984804791ed960479580e9771f24f541c3381fffd307e71ae63857e7bb17e8)
         (y
          0x1aa5afbe24dcf282a7a020229942aa033417fe4f0c9842519006e6debcc1effabe2ee178ff94d847ca3f8798330020e)
         (z
          0x25a3cc5e6753a12339d0733f26884ddedd135df2c09dd8512101bb1582b0c79f130026d5a0656fb5bc7dd0423e7392)))
       (obtained.affine_point
        ((x
          0x14179e16b65b5d0c309972a8be92e1acf5fbe50ecc5b2f6b94e700572d5e69e25b34dc710feea21ecdb3d9cb0c59232)
         (y
          0xc6fc8a1871b48a4c4f51bc11e6059693af951bc755ac7a7271cf9f658fb7777b4bd15e90f42f241e4386b08a332488)
         (infinity false)))
       (expected
        ((x
          0x472d5a8e34f23fe865b0c5f1e9a3885c34e7181a60b1968198da5ef1249f8656ab04a4922913aaaa5d1d7bf1f9d60b)
         (y
          0x6f1a1e406f369bbf5814a1e27c319912097dbcd678b1ed9540d249d16e9311dfacd5b55b2e2baea6df3e6767db8e99)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0x580f48792fd0cdbe9b64ea03ed23f09c5d77033e5b108bb90cb333e4a3dd2d14dfc66a7e0c1d1a9cfcd1b185322756)
       (z_squared
        0x10c79faa52aafa4b48ce0349d036e0c2abfd1f2d012129b134cca577151145b52f2d83daeda303bdd3cd0155d9717cb))
      ((input
        ((x
          0xab689e5d93064f745d8e4bdd9c6cec15e4a79b83da3be5543a11c6d24a36ac306240c38931915caecc8cf46f4919b6)
         (y
          0xb256beae17f35ae57ba57e2abeb536bb0578405e80fd6e1051d2e4cb6b7aac7fff9cdd71244a17f187c5d69829becc)
         (z
          0xe057dabd321d61dd88c25af566ce886aa095d552f87a262c91f1a4e41df6f743585f1056f30a99059456bca8a948af)))
       (obtained.affine_point
        ((x
          0x10240a046861bb53be1325f1bb59958f43603465d1c7aa219676271896c46ac12803a4aeeb27980b66ee897817a6893)
         (y
          0x32344ae7da0fe8516dd4d4ade3c3dcb87b0eaa9d433e1c911940e5f38377bdb6170bd36df7374fc1a4c3600273e1f0)
         (infinity false)))
       (expected
        ((x
          0x3d9de80390a2c267af2b61ab01830451a64b6413b42ec130e7698e77454361401b10159e50d5fda63fd6e78986d67e)
         (y
          0x84434ce1b94191410b54b9e98bec684d6ab23a6e239f6991c7fdc00d78c9adeb492e664868c4bbd1862e2a35036191)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0xb616df91782463bb908f22ca8dde641544ae68510346a561e609fd31ba5cff19f2f0a415aaac5e8f942bc4681a9345)
       (z_squared
        0x192ff7af06a2c1605c7da8e66fca41898f2edbac171097a733f1896a9992353282244a6b009d78a4f9d79b8fd2cc87a))
      ((input
        ((x
          0x129ecf3991f868884a8bf047e2934c3bd82d57b17956bc3f42cd36fabae5fa1628dd41134538b657e40d886a93674f6)
         (y
          0x85d69cb23b496a89bf34101de738b28c08d8e6dda8b58113077cefd8c02ce568102bdfc8c3addd02da63ed9aa43700)
         (z
          0x3eb0854b616c47992c9e2ec934f9dbfec12f600ba91bf012748c1213d96bf700c06066b1cb230397e24c750589c425)))
       (obtained.affine_point
        ((x
          0x14b1520253d871f41968b44f70efd4ab5d17aac6a2559ffc19171587b32b8d4a2f6e88d223cc03d298bb49dd2223172)
         (y
          0x13e96a68e3449dc87de15c7cba201fc37509d428ccf12e79cd8b826f11849061bf98d2ea27e489c86649a6e98fb3e7b)
         (infinity false)))
       (expected
        ((x
          0xda6934eee734b5778d9723138ba14f82680ef7a82b0c1e2e77645af560dd14f1e3151f1d212e38bd94a1e5b30abf60)
         (y
          0x113fc173623e1e56071ad3b31140d917c47cd089b7384f3e4ab9ce3b305b69d39d49b28cf89077591c7aa9ce69b43b3)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0x6a834783e24ce229316770f0a815d23bb8fab53fe13aa94ab72e085e3f2cfb8d48555d8a2cbaa8b3e8b01a8ae869b)
       (z_squared
        0x7050ddbc7237398838c2732b422922c6bf575381c864fa9843c767333892f2bcbbaf252a0c507251661a147c9dcfdb))
      ((input
        ((x
          0x12c40b6cf3c51c6547c73e5dbff93d8e87dca3d689c250e0882fd5b87ed4a0bb7baa796a415884b871d81cdf52b8a56)
         (y
          0xd7306ef4f1fa05e8c785a599048ced5a374c6f1379129e2e4775ef5287ee7c70a0602ac68316da499f2ba398a5bf15)
         (z
          0x1ae3a4617c510eac63b05c06ca1493b1a22d9f300f5138f1ef3622fba094800170b5d44300000008508c00000000000)))
       (obtained.affine_point
        ((x
          0x25c8488cae096a9bce7941f587edf54f06d575fa23add8f29b7ce671b1c4b517f5ad7463bbbbb6511762ae05218b86)
         (y
          0x162870397bbdb3c7ef41b153ad41fb99a5a66323ff809e8060790270630ef425c0a1b59acc414010c82f2d6cea8bfdd)
         (infinity false)))
       (expected
        ((x
          0x13cb45fd8fee68ae50730d39c91b54da9bf9dbc6877a8f15c8fe29b824d37014d6c6e683541f94032bfe4365347e714)
         (y
          0xc64f455e60d5361a959d9e812fea85690c467b673a48b5fa5c743abd992d55cdd42c0356f96c35492165772c5bb915)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0x18f1e50b98c612eda3eeb32488c2a3293fbd916ab7aad8cb928d28de7427cd35beda4ce10342fb76bc5fa33605232f9)
       (z_squared
        0x7bcd797b85c744b027a29de88cb959e552567fea8605c5fdcb3a33e58dca4dafeda7d7a684ca26a4012da54a8778e8))
      ((input
        ((x
          0x16905ac9fecd2c54dc758ce0d1d68922e6b0006f7171abdf44ae5f37effbee450022f92a944a52c2a9a019550e5c9b7)
         (y
          0x433eda1fec0d7a775a7df9557187ca6665a33aef50c6099597da13f6642cf306e114daa931248740b4e741973360e8)
         (z
          0x1ae3a4617c510eac63b05c06ca1493b1a22d9f300f5138f1ef3622fba094800170b5d44300000008508c00000000000)))
       (obtained.affine_point
        ((x
          0x159b3bb1a59b411735a6a864a2da6e258933cae11fbc51c8616aade7d1abf4ed84977ddd906601bfdc7368c27c0fed5)
         (y
          0xfb273f42387fbf626057302465edd1f5cfb563fa618718bd3b1b80a224e986935f331311745e682b64dd0abd893b82)
         (infinity false)))
       (expected
        ((x
          0xd12190c8a006783064986d77a645d31e2c2d83778ce3e4b1377eec64ed6572a359d89a3d260846ba90e79c2824610f)
         (y
          0x8c902c718b8fdbcff7d6d6460b8cb3ff184225ef852a65372f7f84800b2f59ac80069bd7df141bf16fc122d4fcc5d6)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0x105d62854844f6b7281f6558dfe5f4c1db17de00c5ffd92e38020978857c921ab5e385009821bfae03a7cd88d858a0a)
       (z_squared
        0x1202c7b1547934a4bd1a201a080347a27176173f235d91c0b51426fbb513b5e2f4a46226041513a6448f4542f758bbb))
      ((input
        ((x
          0x16bfec0e2f4419bca2f5233025aca937548b07ab39e788bf5b6fac8629eb569c4dfc0bbaa5e8a1d2bc9c27c5d9bf612)
         (y
          0x17b1ef951d7d7ff633900990e776a43cc875fd5c45cc514bac50acd918d0ab6b10230abdfbf1934eba6202c69136962)
         (z
          0x17e6cfe0c32422ea39b781064a97e55d3bb3a73297ba714c01f98139c255e559631afb5f11bcbabdb6e36eace85c049)))
       (obtained.affine_point
        ((x
          0xa0456950ff133d4708c70d9903c2e5ab1610ab1d0ed05cb27059c7bc3aec974a050f9193d7d5b3bb09675dd520717e)
         (y
          0x4879761849d2ccea6a3c5e43243f038b7b6d9cc1a7d6ae0582f02eddc89567283471153ae2396361016b3dcd231dbf)
         (infinity false)))
       (expected
        ((x
          0x6edb723353a105c819440dd09f4cdd897056eeafde261117148547918d6b27b03625ae58eed9ec5f5424e6d854b683)
         (y
          0x6aa0f64aca4d01a651756aa7a58f41455234b764fb46abc33aa3876056fb4b2cbdc740197671417ff05c21919bf5ab)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0xebae556aad43fd090b3b40aa9b6c6e0e78514a0c5aa22336ff45e6145035c24dc0b7c5dd492e8d8748f3786ec1094b)
       (z_squared
        0x15d16a5519e5c371bd8eba673217f2bcf47a84e0c270c2490e0d3573540793c9420c12113add6c4dbe312a0468c8c32))
      ((input
        ((x
          0x17bd339f4d3efac51331de5de3cf0a0690d0d3e65959c5aabd22ba876d258b89cd5ef1718af6898debc584b2b814322)
         (y
          0x3563427ab1cc0b4d974da632539bd1658dad84219db56a8bc6ad0083a80ed094f15df5c2cf93ba92bc4f1a0cff681a)
         (z
          0x1442d98628d01ad4ad48819aa476f4fe76d24950ac0c0afd1472316e118376d99a1e5d56765214e782c4723293dcd7f)))
       (obtained.affine_point
        ((x
          0xa9ee2c537d3e3d7d0cb0bebfac5afeb59d2ed489e7e8bded634266024965899bb2db9ceb6e4c9a08fc31ba89725804)
         (y
          0x1870e8aacb5d30fa78e9fcde57773e69078547bc1042f58924cf163c3c0acb49105b036b339aa5f3930a6ed41facbcf)
         (infinity false)))
       (expected
        ((x
          0x4bfbb52e5d11bc0852c37cf0a09066b0ce426e0ed128d31d670dbdc39606dfb0577e4c0a88bc60e5b5c964a20dc2a)
         (y
          0xc8e0682ba68d3c77f41d3b116528aa07dfb400cf3f0680975e6900b3042daf6d45529d531fcd021bf06500a856519b)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0x1a93a4553f95f23a5aa765d3e61ba1f0a0393c853bfeffa7b49d21079f1495f825269f65a3093b76fd9345c801d3272)
       (z_squared
        0x5898deb38b88b83f891374d8d20217fcab8166efb259919573fb349e3f997b26799232da430ed8df5b0728321aebd2))
      ((input
        ((x
          0x18af27886d83da2224e11a6f8774ffe702af93a45d151ee331eb7c6310fba80b20f7df44a4f008ace461f7fa8959ffc)
         (y
          0xa1802c45da24c00669d4f552dc78137ced76b8e99ce26f28a4656f65a4e5ff79096b46e9bf0d90f1216cd57f596ff0)
         (z
          0xbbf708b580ffd912a2d8ca9bceaf93d1ee171efc2c269c4cdd396b83ec9bf547e543e07a4fb253b7b1c76fdacee922)))
       (obtained.affine_point
        ((x
          0x13c71fc6cd2176ebf78e1c80e63c4c8a05062526afe6e2420c147d312ef831a814a0f1cd2e01596400b333177cd48ab)
         (y
          0x7c92024b367f27d5f24073dbea49efbc47cd394b5201986745ca1437e6e4c65a2c0afece08b167785a3d6c4b081f17)
         (infinity false)))
       (expected
        ((x
          0xac63f37717316945e9a70cda8ceb5cd251e50e7a58fe735828e1c35bd1458760a4ca704efb65e6f0a8f2f1cb9099ce)
         (y
          0x918e23346463c4668544ae03a6a5bd1cc7ca02cb1c47056e1e1f23a0465c00eee938d2cd35392e9b0074b2f6ba9ca5)
         (infinity false))))
      ("z^2 doesn't match expected!"
       (z
        0x74550b6d390b6e7e28514e1cd7d0c24666012f07d48bd5f0db5d6386a01eb92a491a8e49eff9dc54ea4fc1b4f97b84)
       (z_squared
        0x9845c69d6f40c1d6b65840be21cbde16771340a6bd3d086a763f2044253697d834dcfa6090d9b2209f9330e2189bcc)))) |}];
  Option.iter waves ~f:(fun waves ->
      Hardcaml_waveterm.Waveform.Serialize.marshall waves "a.hardcamlwaveform.Z")
;;
