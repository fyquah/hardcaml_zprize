open Core
open Elliptic_curve_lib
open Test_ec_fpn_mixed_add

let config = Config_presets.For_bls12_377.ec_fpn_ops_with_montgomery_reduction
let latency = Ec_fpn_mixed_add.latency config

let%expect_test "latency" =
  Stdio.printf "latency = %d\n" latency;
  [%expect {| latency = 255 |}]
;;

let%expect_test "Test on some test cases" =
  Random.init 123;
  test
    ~config
    ~sim:(create_sim config)
    ~montgomery:true
    (List.concat
       [ (* Handcrafted test cases. *)
         [ ( Ark_bls12_377_g1.subgroup_generator ()
           , Ark_bls12_377_g1.mul (Ark_bls12_377_g1.subgroup_generator ()) ~by:2 )
         ]
       ; (* Randomly generated test cases of points added to the generator. *)
         List.init 50 ~f:(fun _ ->
             let gen = Ark_bls12_377_g1.subgroup_generator () in
             let by = Int.max 1 (Random.int Int.max_value) in
             Ark_bls12_377_g1.mul gen ~by, gen)
       ]
    |> List.map ~f:(fun (p0, p1) ->
           ( affine_to_jacobian p0
           , { Point.Affine.x = Ark_bls12_377_g1.x p1; y = Ark_bls12_377_g1.y p1 } )));
  [%expect{|
    (Error
     (((input
        (((x
           0x16b3891f59651a1e39dcdee5571670f267f1c1cfa92a8132d8a36926337f7db7001977e9a8f5e38b84496e2168f54ad)
          (y
           0x13e6fb667e9765f67e0518e0c79d5fa5f741ae6ea8f6292bcb7ae0c9986f4fce93b0abcd193028a53d0e7b6662cd581)
          (z
           0xc0b309c896b74b681a8041a76f12fcaae03e8690b6cb20c648a6177cde55a407073d348b046e7359da56dd6d873870))
         ((x
           0xed453141939e91056edb5a4b5452ed7e61f7f3dd2a4b7ee90e97c9a2301955880661656781dc90857aed6d6a416390)
          (y
           0xcfb0b9717bc8e5ae04601813171337ad99cdae42c561cae80b12f135c64479d6a23f5675ed5ca7e2dd5e8727d7c7ed))))
       (obtained
        ((x
          0x1fcb371cda9d45827bf63362d4c400f5c600aa82d31d549e1af4e3115b62d6cba4ab61dbeb5334ecbe60a5843cd9c6)
         (y
          0x136bf771f70d601293e02da30b29f46bea22a1e697a3eb57314a6fd7a2fe7c94a6ea0216b3349f9297dcdb432a9326f)
         (infinity false)))
       (expected
        ((x
          0x1252b781171f507db36291b433a1f911a46543890a20ca9712e11f66a5d216e63d817bd8d96cef715abc604dcf6ec2e)
         (y
          0x14a00fa77c727e8987cc438b51bbe012c823a19955ae692c54ce572a61f0ea1fe5cd981533df419fd1330d1f6e6d802)
         (infinity false))))
      ((input
        (((x
           0x15061591d63830f185e636653894077029c0b58dc5a2bdaad8375fbabb21a08db761af82dbf1c0c71b0de680fba705c)
          (y
           0x43bb6f490c82c187cc44b6d546b6b335882f5e6ff9a591ad901c20da9bf9f110ddfaa19a7a4445b37ad608083d4e71)
          (z
           0x10596e793ceeed253577f5b4ecc8f296c391aa59658a1cdb9ecb2758070e6d53bf61a2821f5a430e82782ce7ab7db2d))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0x168e03700c5cc133cfee40efe6349a62c0396382eeb5cdfdc2979fb05061968e4af69c076a53149e957400e951c7dc3)
         (y
          0x14a26c854f0c1d91411528abe7d437dcc8775a85f3059b4e1a2c3a4191284bf748d8fc38f734181e208b7d28e5d7af6)
         (infinity false)))
       (expected
        ((x
          0x18079d3bee177eac492492fbfbbe3d4047bb71325c4f9dd162797622a475f51a4bd4a26f8fd8efb42c17bc27e19c237)
         (y
          0xef817ec99e4e4efc5f2252e43b358b03e29bfad97945cd336a0336c7a918ab83e1589eae1a35f987d0f071ab72313d)
         (infinity false))))
      ((input
        (((x
           0xeb3c74d19860b1b0764311a98bb1e3f10b5c5d5a59cf67ada49dc3e06b4cd640f9a3cef11c7f1863ab7723d9abc2a)
          (y
           0x13ab2ca1a9bd78cac8cee50579b5c88098ab74c2a2cdf107bf2a7b1f3b165a87391c99ed11adde2756eb0ca701ef936)
          (z 0x1))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0xa3635b3d4dea5f754f5cf08603235715245fe1b29fecfa8daaaad4e5419af2e09e7d45b7833620b12beeb388ef2721)
         (y
          0x10d883688e1746c079d52b65cc02ec306fb3c00fe03be1af8bdc7bc3dd8db62107e50eb1dd7473ec9d8e3193d3026bb)
         (infinity false)))
       (expected
        ((x
          0x887598b3dc18b5586417652ecbee8839cf8747f6ce86c5699eab028029a1b5543c20b5556055d0f0ae4b52b783a5c7)
         (y
          0x61133cf2fa34f61dc6ce21aa7312801ef8ba42debbc79425b06331a6d510aeb0ebf7a3c0744e3186f7e8fb5d6e43f3)
         (infinity false))))
      ((input
        (((x
           0x1abc9b2e3eb55ffdf7b541d0c5d3e36a9fa95a8eb979846c1e406dca66cc7c4aa146938b60b15334f9aacc2fea81ba5)
          (y
           0x9f6086b950e97b710e5f37a8e2fd88242b216276401d3dd0202e330caaaeab06a5dd21f2e695c25f1638d5c95be732)
          (z
           0x379908dfa4285b5f04533122edecd7c8dce653f3d075e22adafe0dc944e4c7b339f5684bc0664fed98d0fc0008ecc3))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0x15d3ac174ceb64bae870b646713b1f29029749637e502cdc24de16f385eee8eeb769d3305ca7b22cfffef2ad3dec0a7)
         (y
          0xfa9effcba0f1b104045771897260f9f1c2e7733c88705ee919a213f786788f1b4d68c6bcee30fabbc38a0edb271778)
         (infinity false)))
       (expected
        ((x
          0xb698bf8d86a6f9523546470178bcb8bfc5fb5e1ae09272c243b75eb57793ad90431c7c4c8d93e7332e48f28b84c319)
         (y
          0x5c0c5c4a36f748d5210b634001829b737042162b671f11c21a9d301042b7b150beb5ac35fa620e5389be71a2ad0f9d)
         (infinity false))))
      ((input
        (((x
           0x155ded743109b5317e179cbcd5605c9260398e0cab7d8d8e3888f3333b10f2219529466daa705ca0af4ebc190e5d274)
          (y
           0x3de858acbf87c40862ca6464be21e074c51d1c9cb1243fea67f6e5aa0cff184da73304f39d224919e330f1ed86d8a4)
          (z
           0x3966ac018896752246062ac2d93ccc8dc3ec2738ad87d3c95a536fb898f7640bc4ac1c08323f43c4e8b2d30d669f7e))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0x1569eb214a593a77ad99743472b970b40a4dbac2cf3ac848733406978bd892da55bb153c46c0f721b40d99207535d6d)
         (y
          0x1302e6916ffc087f370240d69bb0f6bdee2b05b4b872204e4306536555d99699367b0f59f436bfe9fa88173875075a)
         (infinity false)))
       (expected
        ((x
          0xbd8141f839770209f7e701221be67a5c4d6294b4561a055b67b87265107d09a17f25d6cd9c4e136e01e96bf1bafd7b)
         (y
          0x8dfc6f219277df7922fce8b63218334192a639a62984a31d10c21d8bdc6b5e66f4196faad764ed633b68fb37492e57)
         (infinity false))))
      ((input
        (((x
           0x80cd14c8b845e34430e4e342ff31a5be4a47aa5cb91eb4c084fbe6c5dbdcc4d19fb78f6819e016280da0dab035f3f9)
          (y
           0xd4c5b34d7025af818588848ea2338ba17882e8dd3bb0307ec9e87cf06a35e0408271872235f5007848b488ea29eadd)
          (z
           0x72aef989c5179e555e6117ebb42703236a8373a0b2a94012f6f8d9fa064b973cde24897387e0a17573a195697c4394))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0x2555aa765dc04aedb51edec1c76f36f443e8b63dcb7d0abb1c1d343c5a4c5d4b3d796ce28ff032c0fa49640307f17c)
         (y
          0x7920d29988915850679a495f06497f405d9c6e78b2127b966b4e72a433df3aefec255cef341b034b78e15b8a53234c)
         (infinity false)))
       (expected
        ((x
          0xb241b51f7f439c098b6a6e7d1da2b29ef9fe7cb00ce1f4dcfd47325fb1651106204e5e36348bd3396eaae8f7b99b0)
         (y
          0x5343c265cd478a3a127e27a2caef192f9cfd32ef83e1f558bc6f284634c494fd1886491662e49dd88c4abc9977a3c)
         (infinity false))))
      ((input
        (((x
           0x111081d763aaa220b73fa5613d5d1aacf2d0451427d8b49d6005c798d32f96a4d7ab3288bbff50d9eda880c746ba150)
          (y
           0x164e3c6731c835be2c6928d08b2c1f3c9f87e68794260f7b4669a5a0859f655a10c54105eb149f1c979530096303a78)
          (z
           0x198460d5a76967b2d6360e482f63706efa3d294bfed222ca5482f0d7726600502bead954f37885c32f0139ae2925fac))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0x149b18f5e06d8608a391ff6b465dcafbfb0c05b461f53d36252b29667dfb1890fcef67d48f75cbca26b20e62d7d3010)
         (y
          0x10e50c4b9174b30bd4b17c7f456c28c14c08d6a89588c77f8f85f14f32441be605c52ce7f78c58eb30c26132a0427e7)
         (infinity false)))
       (expected
        ((x
          0x11258c1fbe9ad2005eca1f02d488435d38f9c59c66263e86536d13c214706eafaa5e85e8be23753e9765dec91770083)
         (y
          0x62704d5166a27665ce640d6fcaf95c496edec707e1559a9a8c18130f1059e51e4c354bea8df06cd0dfea1cfa86cf96)
         (infinity false))))
      ((input
        (((x
           0xcd68d36af9c45d7b201a58c46d20529a7617c0a8935e7380fb54adb8c375264d371444ae0a77670d0392654c0809c6)
          (y
           0x52591d9e4ff33381b1dda61be9f23b287db1de3d7df4bf28b7c85bde48c091b7dfe50aefd91b4f63ef751f183e0a9f)
          (z
           0x1ae3a4617c510eac63b05c06ca1493b1a22d9f300f5138f1ef3622fba094800170b5d44300000008508c00000000000))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0xaaa0030abb9cc8366cf3e8dbd7e2633ff15857c43e1025c180a231a861f26adf208cc792d2a15a77e7d31e3757010c)
         (y
          0x1b7af3e9aeb6c87bcc49cb181d84caf9100c825357a1b4cc6dc874916ffaf8ecf4dbddf4aebee22a9bbb10c5573927)
         (infinity false)))
       (expected
        ((x
          0x4834c90da6b0b6c43f1fb35f6f78475543e1812a9151bf21dc4af96d07fa4febb9867934b55b1bf664fe66c13ac385)
         (y
          0x54b10def966b9c82813143eb96b1f4e6df6ebca08e1e5c84b1589a1b868f0cd0a6dcab3c9dc6d4b161ec027f67a199)
         (infinity false))))
      ((input
        (((x
           0x84da6ae48e89d5a56fb88c02571cc120653bc78629c1eb44066b46c295e341b51dded9ac142dce7fce5a99bbcbc1aa)
          (y
           0x14d5f94f5663def4de0b4e4e81c7a76f9ffd5278e4dbb7117cb5cfe333e88609818e5e975df48b0e27283176011c09e)
          (z
           0xc6fb93c0af23a350206dd76a4ab4fbee20c00be300f4630ee1514bcbbb9e96373789a5c0da51a446ecb7d104a6f42d))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0xc55677187d4016a2ce32d5751b404a7743476c6798c435950a1dccabb0b2c254cb0150877efb230fe6ee392a58a3b0)
         (y
          0x5bae2cd685174b600174c448f503a577ed7d9a4cb8e5fd5e49c3b73f8f89c9a61838253ef748c26681a8e7c0640e58)
         (infinity false)))
       (expected
        ((x
          0x1a2660fb2c9d549a268dded0097cbf50df12ebb3e97ffb3c7431aab0372dc49bae6f902ef62ecbf0dd3d59ba9cbab70)
         (y
          0x98fc08e846b590ae2cfa59a0687bcadba408ac76c008a611c5fc2f35aac9e0b4fc0d3bdeef0afab72d9979f8ba6a9e)
         (infinity false))))
      ((input
        (((x
           0x2d31148ee48ff5a0096ddae6d01b4a13f38317c281ab8eadadc534df8bd55af377d619069c4a252ec1b6f1021146d2)
          (y
           0x291588776629edc4a50375cda388f0ee720f4dd9cc60df799575509b835983ed928bd50bf68b5fe9f15f09c9dba4e)
          (z
           0x108600112d0b8b7d3f73df97e6ec66950121d465d57f49e4f4aa238d94a6f68a617301c063a9da39d363f3ff820a66a))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0x9f8da0d7d5aa120796bf7e6950412cf73e8a19c3b895f5f309b775e8f8242e493f7a8bb4a869874822c3634b0aa9a1)
         (y
          0x1867a06f9310c1c51067854b601d44b635b460f0ace35feb7f0b62d1b72b50c6334a7ea1f3deb4da424b4cfe301e98a)
         (infinity false)))
       (expected
        ((x
          0xc1e3265b5e6a7a84a76fecea35f91d9fe4b9edf99707be6ca473370d7d724c2e53433c61ab0a7f1b77e2ac58bda8a4)
         (y
          0x10d1aacf0d663d87037658453be1b6dcb66341c8ea9b95368c830048e265c287ce39c93bf633ada0eb80f2357d3f71e)
         (infinity false))))
      ((input
        (((x
           0x17fa7bd66f7663c8b5f5f40f5cb41d2bab0963bdc28a0b4f2fbdf01697b44948620980b745f9c4c7d664ce7138186a2)
          (y
           0x7cb01ccb9ddf14e00fa4898db6c52ad74e4f39657e5f6492544e28ef0fbd4c376f6d30d182f52888f058f88afc286)
          (z
           0xae550b36703dcccc31db7cafdc16d71f885c06215b40ac17b2482b59366268cc09d4ce6fb5fb2a12377841cb2dff4b))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0xe127edb0257870a84130d081a3dd04a65ab7e868ff5f312546e63fa780a77a08e151b7f49b3604f6c63b5a8aa820e0)
         (y
          0x40a2b8bcfcf8510cf487dddcf2eb11e9bd6f8783da9373dc57fe4e2d9240a72f8c7a66dde5eee1834e2379f036319f)
         (infinity false)))
       (expected
        ((x
          0xb8e19774ce89b055022b81737bf68267bb7be03f8341e9ebcc7f026ca36ea6f8540c293fb691286b2b931649917ac2)
         (y
          0x5346ecaf948dcb132ad8a372fbd4542e817de0a5b49d30be485611cadfd65b7668e5d5a5a29fc383f36d2581c8a33c)
         (infinity false))))
      ((input
        (((x
           0x12e827de72130dd684471b1aa0c3d3f9afccf3aedd400e0522a7ece05bc5b889a6c75736880684d68e44f69980e9d18)
          (y
           0x1773eda0e7360ee10f747326e2882a66a0b9750e4b3a7f470c5beefd27f783b7dd928ae5df7c76d412013262cec2f05)
          (z
           0x12668c0eb26aa1af1b9fb4ff31f00daf7d580c2bec9c6555ed08a484bb8fa89937f11510febd8fa25401961dd6fa449))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0x2757d1264df0b8749bc14741a64538890d2e9646e0acbaa5e44ab829146ea9a96b558a89e53152cedcfc88ec33de09)
         (y
          0x124fba9f521924451dcddeae6ecd30c47bfe0a6043581559e54c59b295d53d51b0238c66a0f2649d84bee0ab6855d5)
         (infinity false)))
       (expected
        ((x
          0xf76777b246c7579a9fb01b2ba9f9a2fb0f7a42bdf8fe90ff9cff9c4e9fd00135b1e8e92154e1a61a89921e9d9bc934)
         (y
          0x4d8fc7a30fbdbef9f6c9690e885355a6603a5dde14a70f987162b68390f484b7c338c357a023a4cf340dde50993645)
         (infinity false))))
      ((input
        (((x
           0xbad9d8514ca6ad79293bf3e5e59322a2138c7e562b82751304a782f33a6b6862247b1978876d510c5bb3f43081e424)
          (y
           0xca6235a71a46529379e641415010f35b0d3f3926468daea6870d4dedaa81a0ac19006fd7418ab62a2293c163e14a84)
          (z
           0xadbb75a53d00d4213eedad14c93c3617e49a600fe6aeac077646a5900ef86f0c5158a9e10e4a850f7c62491a583e44))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0x103cdbdb26e3481a774cb6fa68dcf5d2d4d340b9142aea9a55ccb9b667101c6c74165dfc98732453ef1c7324a5f6978)
         (y
          0x7e29eba7a2f272ff5866970ebadf1b6b6854fc57a6f02d1d3a1b909d7eac6ef87b43d7fbdf9d21b95809c02d5d87b4)
         (infinity false)))
       (expected
        ((x
          0x16db948249026ad10acfead5ed8e814210204878f9ddb733c1fbd9a9501d6a17ddcd56bf204787df0ac6d4163bc5232)
         (y
          0x1508927984bc8cc667248991bf7a067fab809a7a9013ab8279f480158fdded31462cf401d9af858bb6df94e78c7e13e)
         (infinity false))))
      ((input
        (((x
           0x935603a97920ec4874d6a6289ecae132b98137c822a497c3cd2b6d23e4f317c09f610913a2abc07c731b03780adfa7)
          (y
           0xc45e6c7b2074e57a2ae23267113940d26edf7afa041f85c612e2fa147126ee4eee3ac708cc94fd7b5b15812fb10a7d)
          (z
           0x4786ad570a0d57a7644cfd9e83737b32c9bee3d4a841fae3b4ba53568f74febb275d65b10f5b74f94bebd0b1357dcf))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0xd36c050f03515ea9a377beabebb6829438d04a0c2e028d4017e412fed88479edbbab2c44474a801a41f96a5ef0d5bc)
         (y
          0x2b13ec0c822ae696fc78d791afe7423080f5cfe0c24c543f3c9c88410e8c8a5afa8842f5f6b68172b9bc807aaa2c36)
         (infinity false)))
       (expected
        ((x
          0x120224d83dd033a2759767b6f4a92957c1d705b3a7503f03dd78f75ee982d67b8e519d930fb9771ca38a457efaf37ae)
         (y
          0x5c9788057fbfc8f601c11f614837ffd011569bc619d5d50e853647573fdd45b490a6e6fc5514144bc04f976b92b809)
         (infinity false))))
      ((input
        (((x
           0x887a154f52c655869713f97fc0d397d1540a596e70bff3fc640ab6006f3eb4459bcc746ea30e37a5ae09c0f2823412)
          (y
           0x466080c13961ffa0dd7df2dfb4831a8343f54e2f1177215e2c3a032f926da406f6e846645345d31c18b6792e02bc11)
          (z
           0x1022cd0dd1748ba67c17ea78d54dd547ef4ca9ef624dbdb945a6444f37f00aaa4530875eadbfa8e3f589f5db7b3d59a))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0xe7dfdfcd174658835dcea24f4b03f0c14628a5909130534d4e4d05fcd8b82ece3f053a19ee8093250833e1d0ae5d4f)
         (y
          0x1283a2f3f9904993681beadecca36aef2128f49b52091d5f80bbdca8c183dbf6bb57a86110b47a1bb99be413fd84e0)
         (infinity false)))
       (expected
        ((x
          0x14dd71b6bbc74d25bea17adc6b248e4b2e11b0d2a685aa1a2489dd14dda2f5f0c46f193f4193d779cf2d8590db26b60)
         (y
          0x1b3227e141825fd3e60bd2885491fc585f755c39c63f1c2021d192a9744053a979c1f67bd3991b328b16677d190687)
         (infinity false))))
      ((input
        (((x
           0x85113975a238f9e10ed66b310bce89648844150a0705d951f1f888d53e8fa44c4941f4e0ba2e883761f4db2c6efdd4)
          (y
           0xcb3111b265139f5addc13f43ce29e4d2ee049d95427096a92294049c27769c30f223ebd4920ecaaf7025d68da9bc30)
          (z
           0x9780282fb31e09f7596667b19ecbfc010cb1276611e665d55dcc6e88e2d73ec159393643d720c92423cce78a23e162))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0x14e63d95667058989832fe2530b30ae2c45794a9b95f14b4d105fd113cd7a84682f229b47ebcc7b6bf30dbadd821ecc)
         (y
          0x187ba5ce99866a34cc69e0e479ce92c1ff55ff4c8154ef330b0e2f987c38c783a8350bd0aff91cef0082934aeb461f1)
         (infinity false)))
       (expected
        ((x
          0x46589b7be84078cf5cebc664d65fd4aeeb429f1adb544dee65e9937ba1e7c86890de9fe5aebb6d72cccbc25aa9c316)
         (y
          0x12cd53f9ad1b4d4ec3561835a370fe972659a92b71b4721c9e1343193f9930144203c2f7930d516e0ca6f7abddbf071)
         (infinity false))))
      ((input
        (((x
           0x104c7f73f26e20ad6230cb2c635f5791cd094da1cb90b88bd4d71b9bdca03fd0a67ff7f13b7da61097208db88caa422)
          (y
           0x40f46d07ccfe207def827e773b583b2e8a076a20d6d34077f347260b6d1222a80714a460e28faf053525807ff27a40)
          (z
           0x10f76a6cedc282004c992c8d2242ed3149c3faf94a1c32842f87abf850dcc437bf1e1c7c5c5c8f64b03c9c8affda557))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0x7f97a1388b7f6927788afd7f78d49f02562ee51bb3df77a7789f9a51bc968f19c9dc2bde46575f72022fa67486a569)
         (y
          0xb70868d5efff7bc8351ec196db9ce63cd0d13fe7fd067b36277ccf391a6de0739eced485712d2b7da5e72ff77f84d3)
         (infinity false)))
       (expected
        ((x
          0x8ea4a594c981269a445fc662f7cb4e231aadc68ebd955f8da7360916e8e381613de5874e6b0b465c6d93b32675458a)
         (y
          0xd0baa286d8e17ce79ccfce27f703b612afed40ad65d6df4d646cd02b4ee4b684631a092907b4dda04f03fd5ebb5637)
         (infinity false))))
      ((input
        (((x
           0x3ce29aa5b54b5c1c982bd15a85dd109af563ad50a2a2b564ed3a4d2418f3c560d7957d14c31fd295894f7e9752e136)
          (y
           0x103ef6a62a3c87684b13b4f61e1457b808ea5d87b7e6ddff4e470f0b7f60f1dd225b87f21aa34ed6ed795d9274b3e60)
          (z
           0x5eeec2961345bc21014faeebe45ff20693d5c98c593cea86701b73724adab34b7529f0a3ae7d2504168c30d60dbd57))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0x155e3bc6369a5f66c57bbc6a5c4fe742b577cf34d01c07fb6da145967eafadf2c01547bfe8f53d27fb56908b1980445)
         (y
          0x18cd4b0520020ac9f178a1d93f71a19b1cb4c3cc6e2ccc5a7f3ef8b456661e0d9c658a39fe0727ad365b4d3f0386c28)
         (infinity false)))
       (expected
        ((x
          0xe02deea4025a24ae9e5163da78b35d9ba8f81aea5f110c167aebc3d2f8c2b7c9b705b59e67765b9d4d22c2db81816c)
         (y
          0xe80e2edb98141dc49a8a3f292e949f6defbdcd6d5385cc37c5503b666cb7a5669a5672510c4ebeea498966ac210a19)
         (infinity false))))
      ((input
        (((x
           0x1025eb1e54306a48930bdb2bfeae03831f9745d274a72c0199cc5771e1379a24e91486f6c7576db22df38e4878d0c2)
          (y
           0x7edb9e03fdb57c7207d39fe7092c1c1380e886744467ba14c6df753287c30d2a5a3657d0c351223082f778752fb838)
          (z
           0x1e46dff1b90e6a946b0b4919daa1a507697e7d044e33316428fea46979d6b29f7ee460f43ddf0b62fc68ba5d361e80))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0xe9db12fe76b8be95d8720825d690d3a66fe6e2ca458c31fc4bf2aee20cf5a3b9e893199e476fbd0608db9a8cc4d710)
         (y
          0x1058e361ce69936e1558f91edb4a1a1e82b3929a7899b862151a6682b02a1d44dc9b19fd4954a9ea0ac09021a89815e)
         (infinity false)))
       (expected
        ((x
          0x66814948f02514c1676934f7d53e085603e0920fe9adffe0464a51a36eadc85602638ff274da76b7f406647cf8b12)
         (y
          0xb90f6aeb32fa8ea20b7cc3352959604a45bebc3c5cdd29d659d8a45c8a2eac17d7d211b0d17f3ccd5224837958447e)
         (infinity false))))
      ((input
        (((x
           0x5fd3614873f253ebce854043200454f3476bde973f644e29d593eaeee4eb5d415f2cf71ad5baf15273f6eefc3c19b3)
          (y
           0x16285b76bd6543f4fd4635502abeb88ddb4cc188408277911f2101e74423505c4842a99ad526fc0ff574f148a315830)
          (z
           0x16ae52ecf4c6498bd9cb658eacfc4e03799ff0209c290838be85635c5e04a9d318741e02b19779178e1cb6bc74c201))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0xedb8339ccf651a2d1ed9032d109eff8e48f9da63a0ed3c02bbcd2bef8294cd9943d1f135386d36048a6f09777a81)
         (y
          0xffe7d9116dc804494d2f94a7c3a70c62f11403dd2d4bba4827d7017298cb1ad3bc7b45a148afea66eb64726ecb3ccf)
         (infinity false)))
       (expected
        ((x
          0x14516d0029481022132980bf9d5fab0ee848740bf8f9d60008b8e5e65cbd5363b595a6992cdb733f99339877633a34b)
         (y
          0x167a5d432a0bb75df7166188774bbea9487d413762e9b7954dc225818e4e56209f722e83b5d5e4c4a024480f1174924)
         (infinity false))))
      ((input
        (((x
           0xeef57d8ca3c8b0af591b9228fa573b6505f0f9518c097c3fd1643c772aa10771c072d42e3fcb09aee6174a6e459a96)
          (y
           0x17cd6cacd1b24cde4aa16358865e859ec329c3d1da5d7f15658d30707a79d29b8fbd799d5ee2d77184529da8671b06c)
          (z
           0x1ae3a4617c510eac63b05c06ca1493b1a22d9f300f5138f1ef3622fba094800170b5d44300000008508c00000000000))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0x5245b156aaff822b10d370e43a0bc8e4c8ce0f02e718fb198260e21feb2cb2aa4d5c74de6657e373b00c8698b876f)
         (y
          0xd2f6d3f06db9d4eedb54a7dbb11c54bcdd65b2a2864ee4676c20f874f2196525530cbcba106cceec1e199990d6d071)
         (infinity false)))
       (expected
        ((x
          0x158f6a09e132dfbf53e9a0b79b311ebde1d9e027ae74d4ba9a8ebcf4326fa10478d176d20f254396a6288a41be1582b)
         (y
          0x7fadab1ffb559cb2eb35b7b55f02524bd0adf11d2d013a393d70d425d6fc850e1de5c079327bcf28ba2e4cd0903f6e)
         (infinity false))))
      ((input
        (((x
           0xa02dd4c542ce7ebdde8c17e18a1a5ce3dd0e5681ad89a848388cd4dd4c9477e1c266ed4108baee87fe57d0a306cc86)
          (y
           0x17a343b935bd7779717babcf07001c21b86b2ee25f5f3d5477eb31ab13936733ddf0f53f5f58022efa4446d462ab5fc)
          (z
           0x1081211aaa7024d8191eee89cbb32580dd2eff7fe9fe90f4daa10936ba4a49dd0efd9d1cc2a47e2643f3d86950ae967))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0x8849b9153cbca2dee69941c41765d9ef880452308f90306e36ed83a19a3b81503881f2528c88185ff484b39c9ee498)
         (y
          0x77b71ef984bd61cfdf8abc9bda04bef0c96a0f090d37ecc18be333875ba5c65df50600d68990c00d332abc03785ee2)
         (infinity false)))
       (expected
        ((x
          0x1b29247e5308447897653070d9f26a70759ecef04b7d78ebb0445d999b01f4d5b170a6c29e629bcde7530b71ec96ea)
         (y
          0x1847c152c9e92c301bfe5d4371fe11a7a51ed44a67ede028855aef6a442874d161adcfb95940fa9e40aaee1040fd32c)
         (infinity false))))
      ((input
        (((x
           0x119dc3670fca2a8466a720293d836b38bbae5fcea666c5e5d3fc6b84ce21b4ae996a8474e9efff0ca9271e73af0909)
          (y
           0xb1e25a483820408da3eee1b23d55fee03e91992c8cc77ecb2cec4e4b2957e238e08679004f1cf34ddebeb84dd47a9c)
          (z
           0xc07c8fcda2091f86371b08f2468bc3bf5a627eae2551a4a33aecb33e844aaa5d38bda98e95d130585d1356b06ebf07))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0xf139108e16375b363cea2d4eb4d5da36da2d841330f3d1dc101e18a3433a94a463af71fe74e70d730c739ba1f9a6e2)
         (y
          0x15c3f527bcc140d5f217a2e0dbd7ca9674fb7980dcb088ecc3937c213eb6834c9f43838b52af4fbdc0f8e73bc31515e)
         (infinity false)))
       (expected
        ((x
          0x12af99ebcd27e2476ba5ab3f58a12040b307f1b0dcf513c22ca8860efe1414601b0fbaf6cbb61702576bb5e122d738e)
         (y
          0x9903b7cd8ef777ef918ac7f02b6fbe505dc1baa8cb0e31b2c62b6a5bbe09851737a71f73eef641ee17711741f31de8)
         (infinity false))))
      ((input
        (((x
           0x9684f08f10bdc2c7323230f00004e912a11fa3446ff32092f3e520df55dc1e18bee12c1876227776b7e4edb7b90d9)
          (y
           0x73de1bee6a76d7a9c1faead4be3af2cdc2c6bc5a5014ba787250d99e71d6f0de731fe41a2c1606dbfbbf68812e17dd)
          (z
           0xdd136e48122e124bd83b94edae647238e2d9a69c54ef521b47f5cd46e156dc7c99729ff168be4c417868e468c3f07c))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0x60b90f13fdd8a82b7aeb3b9d08323ffec38081d09719e2aa235ecab9380d81b2cd13ddadf2d3f09e9df5d8563486c2)
         (y
          0x8c3a5988d33d272db2e6e81723f696b78f4bd2bfb7bdf9a9b9de01cdf00f1bcdd682f297c52dbd2ecc7eda95e33953)
         (infinity false)))
       (expected
        ((x
          0x111f7f263c4b3d53832d0f78766c017e574b8232d87219db77d459acbf6a3d32f3c3e10313f6e5aa2638548e97b4b1f)
         (y
          0xfc1177a415260308038de0cc94299139aa5b0396deb0b9c75a95b47226bb475d16f077b1bfdd4f756c83c2ba9a30d2)
         (infinity false))))
      ((input
        (((x
           0x7fef0233ce3edf4524615d26040330366703ce99444d6a101a9f18dbe0ebe729c176b116358219485783e4a34a8cf)
          (y
           0xa59daeccbccfecbd764d6d05011a84e1527d3af6e54ed34f910c57e6ddc2a7f034a940d3fcd8385822826bfed7284b)
          (z
           0x11f246eb85f29b221022996d28aec1fcaac7f0609647b0614d2a69fb67a94ee80091103d0b4069cf663de84799e2791))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0x1504de28f4843a1f85d954512e96612a14a3d52c211cc29e06893af09091e4e48a45d12fef73bd07c1058a2e813e5ec)
         (y
          0x1a7f60f099809bc16f15fa33355c5f06c29c850282110b72bc198897d670f8fdce7e37f4e6f22ef67edcce694d38667)
         (infinity false)))
       (expected
        ((x
          0xfd4dfc521e534fba13f801f334cb8d92bcf63e72b2726c0b31e3b9f8c1173140fec2a6466a638c8f9a3710655871b5)
         (y
          0x14003032d50dd12f5e042adca245ed2a9dd8b9fcd9fc6646afd06260e8f30032db9bcbddb1cb8f671979ccab37b8290)
         (infinity false))))
      ((input
        (((x
           0x171af2b237267c143e71b20ab2f93e73ae15cfd7fe3137e14b1e0d3159f71e8a3227b23651ccd387e9ce91c640b339f)
          (y
           0x13b97f0cc839be030cd2ceeb3d74fc51aec4290405041ad9050dc87bd1daa81bb7dd0e26309407096c20486da24769)
          (z
           0x10269bf3c32af61f0fce7e2c31c834ecebfa166ce1059fc990f5287dab786062aa79d65a12b686533a242e8250ebfea))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0x10030d2fc9cb495ee16f2feaebc81abd7282c572dfc8efc0e7ab32d31a31ce314097ba66040952789f18009ecce02ba)
         (y
          0x175737a20df95655f8419643bf88aa1fe4b0081faba682f1e994944d292e1090146130be5b0c9b448de9021e8883bac)
         (infinity false)))
       (expected
        ((x
          0x8d137f855012f91a54c32826859e914919b4afb5579193f95701034b774ee381cf5e2dc279636884013f805e31a7d2)
         (y
          0x27255c03b2ee214b618673f1becf6b3efaf2b58ca3d422e9661e7a8e017b2dde77f6b0bb50a796a62eb4966c10588b)
         (infinity false))))
      ((input
        (((x
           0x42f25dfce87862301c0f316aaa6658e8320c60f0637d1829d82b8b11a003ab0547401653d3800ae7d07a906512211)
          (y
           0xe0ece7311aed6bcd218e7c54d3daf91321a012d7c5648c634f83f29e65246ba85d7c4895a6b1268161f98b7697084a)
          (z
           0xe95e4e448d99d605ffaeef7bf59c2257b716b4272c93231e434c004e5e50b32b030419b50f122df6609964ee2a94c4))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0xb3453c687da6ea7d647cf45fa50c18a056b1ae67f634b8b2adb5726f44a46162c2ea0464ad6a90b9854e213f0e1e8c)
         (y
          0xe94bc45cff3ed73253f7fa1c45e36823f754ae7f4238d005b705920bc31dcc7b592e36efe31b6fed8360d56d6a250a)
         (infinity false)))
       (expected
        ((x
          0xe41f02349e7475ab8673969cb2bf5196f2573bbfc0d9ff7ff6f638c81f15cebab6eceed4172255c89c07b07b036c7e)
         (y
          0x9de20746120820957b83726d146f79c93c05c28437cb5ec086dd7dcd7e33dea811c111cad7caf65ac0cbfa6d705ae5)
         (infinity false))))
      ((input
        (((x
           0x1ad4d140d6e8e100a1594decf89aa7f180ee569ae781732a05dcd10971714d54ce5b83e9a0d0bcb0ae93208ff3663e7)
          (y
           0x7505f88b310f0e0afbed7908bffe2f3a56b7d19bbb28a156e81bc5c1b08182e455d0eaf1e6d9999cd6349cd167ceff)
          (z 0x1))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0x5a33e7ea9c665690cd0c65fd16e6ea6ed1fb575616ae3b0b2cbcda267d72e099357884686a73fbffc78cdff3f92522)
         (y
          0x15f2bd71efea1e4f3c752f3f7e43b119f31b5145b592fb13124e02f369741c752f37441c4b1f83b62d5c145b1675510)
         (infinity false)))
       (expected
        ((x
          0x36b1513bed26f2c3e911b5d4fc7ca136e9c99f1b8f8415b8c9d2cc65c3b44c80679674d885ebad1972d17b4b3aa8bc)
         (y
          0x2f853e718c00c63d2d2ecec1e3a692d4837fc41646d8d2bdf5b981bcce9b49da2fe8299088d35b21e5b10df31a11e8)
         (infinity false))))
      ((input
        (((x
           0xc69b2b3c11d66526371f8270bfcfe58f260818e292e00b56eef70da81581abaaa0a2bf9cfaa077fa5aff59ced2402b)
          (y
           0x15680216771dea30b63140f5f1f202aee93f25e1a8a05412bc4a25dcf7e1f7df0f065990314a81303c6db4e3ab51050)
          (z
           0xae32a3805d04d3c21aff7f6e2e2da333272c5c97f626dbbb1ee1b230827f2f4321456649cb0380750e5d8d1b90991b))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0x513137da2a9c4eecfb0dd94a813e2f9df789d157bdeef177c2999e242f58f2e5e25d23c2bfbd24de380ec72b67c1f1)
         (y
          0x18b4f976a8623a57c0c27b7f11be63febb3357a1c28e3e03d1d956ededc07c0e1dc1f5142e9895a369a73d5e3c7073b)
         (infinity false)))
       (expected
        ((x
          0x340b158711d9e5b4ce9453812be22a27e1e17e469cc2a84cf2644b48ceb71055fb9823c4300778072a9c8866833540)
         (y
          0x4dad4a81ab59c8077cef8d4666696240c4d74ef076171720c979f1405fccb89bf8cacd257ac1db988533ccb13ba266)
         (infinity false))))
      ((input
        (((x
           0xd46dd21d6980de4cc090f8a7a9c0937b5c1267a12eb3e74841179ee028cfe0a098f31f0fa939e4b85ddee69def68c8)
          (y
           0xb8ff99033dde7ca9731dffe20dd65622641f244b000b5d69152e3e7e1c04910b272eeb65f2e74e7c9af6d713ce7c88)
          (z
           0xdb6f260b498f317345a58391a32223797b8fb4b8c040e07a04bd73b1715dd360be9c2275a566c653477d1ec93422cb))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0xec6958df5c552cefdfb9732255d2c6cd4632f2190eb1a6704ed35b2b78bffa386c86cd2e0c3693c5cc4a29f1ea056b)
         (y
          0x12da85572f013961ff884dec36cdf5c0c789ac17886eb7f662670295f5c5e075270f005ab9e404fc5f41ebc350c2a5f)
         (infinity false)))
       (expected
        ((x
          0x71b67c517b7ad1424eabb5909d5b3b7edc055e8481b3fb5d57b28000fab7e7121471c0743d15563b72d1886c03f04c)
         (y
          0x62cc5f7064db5ff9fdf0b17f7ab3839301b714c77fd5564bc10e99b3e58276b2d9b0ab237d9aa7e62e73cb05ad0154)
         (infinity false))))
      ((input
        (((x
           0x9163de0e36b90c98b3ae64de5e902bc38879113f679fb1a46445fdfe4790ed03060d914c81b136bdadb3c96598221a)
          (y
           0xf5454bd5ce3df600359708a483f6af349953a47c9f09b14537f612a8df91a0ceea5c52b7bc020803c9293e3151e9e8)
          (z
           0x162892e7ecdd549c270dcf0d3439cccf08e9a31709b176e34f3a86b18973a56705ac30b0a8face14cef81ea822de0e2))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0x37b97f63f78a27742246286dd2b43c1119a02ed5af365ad12e145e13cf443214fc7943a6a91d4a0891c10e18d85007)
         (y
          0x797cbd0a467b40df94d0125479b29f1b99c589b04b6ee33a178accf10e859b51e40f229c836899e768523045d79b35)
         (infinity false)))
       (expected
        ((x
          0x33bf7d9559e718d4c8f06127b4f980da0c6a37008d9556d1024fe3e4eef1610abac9433e9e46bd63b0c95a15fbae11)
         (y
          0x9b16427cc2c4c9eabcd7bd57afec8b176d00dbbe3e581a95ea723333caca1652364448f78aacfb0a1ecd387c4831fa)
         (infinity false))))
      ((input
        (((x
           0xb2cded3fdee96980c3c8831ea7a2f6c8bd9c13cf11aa2ecba7307ca84fd53e6a72529b4ab3011f1f63216293ac3479)
          (y
           0x998d4a943c134ff7d1f74928808083a0b9e5785547bda6e8bdd7fb194af8c07692d703f4658e52729fdda46199e388)
          (z
           0xe2dd56471839fc67f85c43daae23b527aedebd52f6aff1d7ad3ed9d91411ed9c93e8dd51f481af0305fd51227e97f3))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0x51ee1fc72a254a2d0ac19c2362b2f125d00d2eda50cf003616f28a2f5a01ecaa5748dd78257034807d9094d9a64f4)
         (y
          0x91f13eb1be70af37c6578bd9e8cf0eebbb47ca4804979ec134efeef00bf453e3cc8331670d27373f43bea4a6c1a62f)
         (infinity false)))
       (expected
        ((x
          0x653bb4b20f4c0c6cece66840e1de380b230d32fc31e57c568f8e908be21253b78ce62d367cf87fd6dcc7bf1c73216c)
         (y
          0x5ce996c6a9c476f2f0efc992cca9d1eb7200b453904857efb5f22b81c9acc7fcc1692f2eb3f4cbb5dbf31e079b310b)
         (infinity false))))
      ((input
        (((x
           0x1730ee03c7fdbe1fb5c88d7a865f13a9ba1e60cc2425a8aab1dafeadf4fcb132b1d38b75aab831e54c71a949e457baf)
          (y
           0x143b787eb7db41248bd700d9fbfa6627c2eaccb1e2447326200ae8cc388649e7b8ba6a4e398f354c06431bd96843db4)
          (z
           0xea59b0cbcc74affa200befc1f0c5ca5079ee38bcac9609f4453e3c6e8248ea1441a397cd5a9dacab1b5143fb89bb71))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0xb892001d609dc41c25c1b34ded327d732e80bca3777ff901d73dd728e955a1b46c949aaea841eef730aa63fa6e433e)
         (y
          0x88b9534f7d07f48f8e0b5d6dacb9fc73355d827ce9b7f6b7ad71b8a562fdcaaeffca30eed8b9cccc2d48bde2a0e08)
         (infinity false)))
       (expected
        ((x
          0x1483ab2abb78f6f689d5eb015fbd9dcd9497dfa268904d177d76b0aeff842df063750bb8d13ac57cc6c3e89284bcd22)
         (y
          0x1718134c1de9a5a721b79943351fb27662a7305aa5099977cffb45b64f22bae8196b21481c64670f102b8c17796bf3a)
         (infinity false))))
      ((input
        (((x
           0xd890eafed3fc61e528220171e961989f57a4512cfef1b335eb28b75faed2b3526f255e4aac361b2e9b7aeb1a113b67)
          (y
           0xea2bffebcf044966a0e3ca9a64a1ec0e5f1d2c4fdbdcfdc962b7b8f9118878ccd9c011b8027e040df1da09c6b552d7)
          (z
           0x1a01a02919854c6cbbca2ae9dc59265ca173ab324175ef7c751fe92c30227f478b1dfaea33b731c8b49c8de2c8c872c))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0x592de065b2dbc8673223d8c848aaf8c4917012d0497547bb6cd8aff784690be25bfe0d633dfd1e3777ba1d368a599a)
         (y
          0x476b48d3a1e5ee6b912648e5ae1de663099861b07bc72000ff6cc317e6e3ba12dd45a1600b1ad7dc60a90d0f1a9a7f)
         (infinity false)))
       (expected
        ((x
          0x19b6414f17e635b06a1809196a89191878689189a8007615b92c2459edb6ff61c83b5122c205cb4eebc45dcbd2ab8a6)
         (y
          0x16c8e2e7d9c98aefb02f5026b601243ded1283ec662a530df015910eaf8a1394cda0357a5c88c3c37879e5e8b33475c)
         (infinity false))))
      ((input
        (((x
           0xcdd44e557c1d78d3b92705e9995625969c2de665659d86ba3314045c01db9e205916e67f92fcea94529a419dd5d845)
          (y
           0x18c45ede1e0e378fa806f5c173233f32a0663c473dd2f0437a8e47c9eb06d5fd6f02b346907f0183968b9403f384542)
          (z
           0x18f63f4a3410e92eb70669477b8e9c13ff382234d1014d1194d06d764054cd372eac90cc91e011cdb7e7749109fb24b))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0x1157f477fbeb21d78ce5dc2570872cd6384c891669da06b64a4a8aa1efddc8a3889e63c2572d504495d3c672af6f4db)
         (y
          0xd4eb9d0e2c8c55fce5ea7bcbf9c2a2eed84edf01a3981a837a8f0f09ae56a08421079e73b0aabc001ee57218398957)
         (infinity false)))
       (expected
        ((x
          0x159c6e37722ae15c4b6c2c0b8efad0aff7d344ae32e9cb3d19d0fd48fea22c878a2e4733cc2f1d25d1f543b0e3b09f5)
         (y
          0x2372d9dbfc959475a801b8e483835ef46c525fdf45ce3087486d747aa65da545ea7e0834c8eb8cf84bec6b6291cd67)
         (infinity false))))
      ((input
        (((x
           0x40b543ea351d700e5d73d0a17c0219d6be4b7289e4aac925ffc68ee76b740e0dd66e5b89f1f3bf2a9e3d39c52b1684)
          (y
           0x10af2c5e041cc42fa44313e3dbeb1d5066947ab26b1bcfc338a1c1273e08f0e0cb7888a6a0896c2de2495f9bf685e93)
          (z
           0xc7f7facdeef4a9612dfec1045224882cc86c6bd773db1f19428a66b8e80801d0a691d0dd965bcb0198dc1f8056770d))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0x181636b89cb5134345644646818a10f7a008de6e97feee1bdfb70d23ccd24d8cc5c6f0957bfd2bdd8d5cb62b8d0f54b)
         (y
          0x18bd11652c9c0155b73e14fd069c53b1f11c5e3deeb7e0588ae647bd485c52c3d96bc2f10b9f87ea0b5a4ee02707dee)
         (infinity false)))
       (expected
        ((x
          0x474b43557c5cc2031243f25be35a4d3ec9397991ce02b9b50354645dbb21a0cc5fff8b5fe7f5fbbdf7969a3909bcc2)
         (y
          0xf5ef2d8ec1598786e9e6369596822da120650aa13414761a63179704b267c31b18f346c22c2f7c3c23040e1bcf5aec)
         (infinity false))))
      ((input
        (((x
           0x11e3c5b209b2f85029e3296a33170263361923fb8bcb4c5305769c9c20324076997e9863dbe36c9d0c0b2ebd1c40bf3)
          (y
           0xbceb9ff5afdab44a31d6ff9fda508ef728d4c01c7d37480df8b1a084eaced89629b7424d2f9d1da80bb73cced8d6c4)
          (z
           0x18cca10d11f3b387b85591e1772e867c61c93e900cd5ca253c6ca8509d1a1cf53010b7380bb44ad16efc249bff55fb9))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0xc71aeb422f5568b2fc89fd59c4e092e2abc5f3a1d1b1a02ca437689ebc38fa28edb52581f08837313cc8db8c2a795)
         (y
          0x5c1abcc1b0eb60033b67886549f8f89188937a3e74eff1e2c8adb962ebdb033e54b734036af74985664fdedba5e237)
         (infinity false)))
       (expected
        ((x
          0x1a3f6b21cee0001028e34de7e382c962688e4f69180370b9e4a38c77f3def8bca7bbcaceab69d4b5a8ba5f7c1d5b2c4)
         (y
          0x542778b404c0968c4aa123defbff328bab628b2ea9fefe725716cb30064de46c7b8c8d6ede0fad484a6c26a7a02dfb)
         (infinity false))))
      ((input
        (((x
           0x42ac27279035e9bb9236b5a2225e81c002b3850f8465936bfc0472f5fa82b2c7b755a792674c42a7568fe10c75554f)
          (y
           0x13d81ac67be0b77d30914658dd31e0d72dcad4a60a800c17774ae63f6c6b2e6df1303b90a8ec50766bb250648d799b0)
          (z
           0xbcab6b4d7ca748d1921ff45df3160a4356a8d35ecef016b9b848d274f48198f74371af13bf682dec95c9a64a2c7a78))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0x6a048730bf58f5187be0974d4f37f96bbf6510cd3447f2f6d06939baac2df0f4d86dd0801fd5d19a484370798f378a)
         (y
          0x5a285430e604bb22b146c1498f15084b5c1d8b07da7f766a8873e5fda5ba2c2c7c32301a46c9a12d3441d776aa3127)
         (infinity false)))
       (expected
        ((x
          0x182e881a83f2e4381eddd4e0d4499ceb3d05af03dfe8a09d82e68d82e2f861395d82d92b0e1ec1ab14e510ce48a4b2a)
         (y
          0x93a9caaec85a5574ca54c2f370ca063ac00174bcecef5034228f63c82686dfd3a3cfab9f0973741ec38ddaceb8bf5b)
         (infinity false))))
      ((input
        (((x
           0x187d65aab2cb0213f90171ed7affcb3df2b6935b9c522a57bf09bc68a7f5f7cb5a270478d5637aeb1743023ecb80050)
          (y
           0x165d4e0add88bfa4abce1f3060169499312a1d37eb3e46f4a003642823f487293292dbf83906ea902df36876ef2bf2c)
          (z
           0xeb0afc47639fac21ad56dae646a869ed76e79805af07750a3aabe0189313f9588a8af849b5c4c1b90497a9f189bd65))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0xdf7349e087d4eec5160dbd770d9ad2c34db007d64bb2acaf6b5b08ee6b97093e09a0c59f9448b218fe720e1eb2fbe4)
         (y
          0x15199e34311b95fbfef6247944dc38ef5ea8aed92486439de8ce42f552cf2d2a9d3f8784d3e45c2151b7bf29a6a9568)
         (infinity false)))
       (expected
        ((x
          0x1a70484e7bb0f1bb0c105881ce8e18ca77a63c5961a1ff016f529a951806b5b9cc809e828078aaf3076d52268c8529c)
         (y
          0x6d30e5e7a856ecefa18a8f0db5a9771a9bd91bd1074444be6936acc79cd065c1476161a3ab3455212ed35630a8e14b)
         (infinity false))))
      ((input
        (((x
           0xb86c7ac4d86c99ac47ea1c5d8d7e3d76f17a9edf9b65359a41868f0225e9162cb874d4281514f78b6268c715108c3d)
          (y
           0x38664f606fa114b0e38653e2eee5064c6e5e3ff325097f3177bff2285631c0fdecffb6aed5d5ab2040dd7b3e9ed20f)
          (z
           0x1ae3a4617c510eac63b05c06ca1493b1a22d9f300f5138f1ef3622fba094800170b5d44300000008508c00000000000))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0x8a93fb2d9418d2030e7e8b1f083b64c5b1435864fd20212d62e01998c64896a96786a2b87d5d7546cd859f14ce1e64)
         (y
          0x1a80fa63fa965b6f317c4ee191b5bbfdb8a0864f6855b78217e0011396da4a82fcfb2a68b3e9c0c7999ae56d2eedc)
         (infinity false)))
       (expected
        ((x
          0x5dc43cbc627d4d81e28b9ef8a7c904ba2f2712f9281726d01b3e1b1c33f0bac11e189a81281cbe4f66a7ad47fe10cf)
         (y
          0x18d82f154248c687ad3f3a876fc7b10986c7120fe09741c3b6374a988e22406250ae508d4bc438782fc8898a84c6886)
         (infinity false))))
      ((input
        (((x
           0x13a11486424c57dd870b17b353f8859d577ae2251d03801f6243d441f0c1882a8a8e7fc31bd9e9c60936f20f7d78752)
          (y
           0x187f827f43330d4307c10f306ee046d7443e996f5b0393316139721f4bd9cfd12c62539f79b01578f0e0547d0db957c)
          (z
           0x1935779a2bfde54ebb4577a469873d130e56c1ebb300b6c31d011bd91cf5a20fedec4b5b5831761b18397ba78d9a1ab))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0x1ac9e159e368c608d7a288c0e204625700cc8f43e90139ef44a5c2e8147d07fbde49a46736b48211adc1beb1a876d9b)
         (y
          0x66d545a5fa34c86a24a41ac7e229ee4059e2c3255b53882c931990273283cb6476aa02bcef0b981265abdc1096bcc9)
         (infinity false)))
       (expected
        ((x
          0x863e9a8ceac6f1d77dc7f23732eb7a38e15d88bc98284569f21a30e1efbc98bbfc847361482fcf7b886142ad980f61)
         (y
          0x15278008af94c2ac840d8cbfaabbe1666d78831b0273de1e55d1630fb2c48dd89ee7edbd42d91f8c620931c2af553ca)
         (infinity false))))
      ((input
        (((x
           0xca39bffcab5e0c04bd3b222aa3d98c60c9ed80c3a22c039a7e369db64fb0f57b58be1d250a5b7038fea6121187d0ea)
          (y
           0x78d86ed73a55e9014945466cbd31c15f5db92842cd1939126d1e53fe2f554e264524e7212ff3f02015ce91d1a1d5d1)
          (z
           0x1f63d6449ec7ebfc5718a2bd94d43fe6afc56c24ca056a0b674309d4a2f4ac5e5dad39ab09278aa093d917dd045f9d))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0x164aa22af4318bcdce1b395569693ee7a695438189689ce28b1f63fb02b8dcfa3e18f4be1aa6ebed6efdb5a8cd9c9ad)
         (y
          0x14e8b2d920a195175f5c84b3e9096c93e533ac0b4e636838a240d12161e42d05117b69aff13f5fc3cc699695abe2f19)
         (infinity false)))
       (expected
        ((x
          0xa39b446f702cfdcc38fcc1fe4eea89325095d2ce8ac1f0c38a68ede6c632c6c05c3680f0a55a6c5d5df7995038f677)
         (y
          0x16a0a0722ebb04c367906d26f471b21de07d83092fcb8dea6fcf9392a8bb5f86e36e413d0d75d916f2b790a6ed773ef)
         (infinity false))))
      ((input
        (((x
           0x18a40f3bf7d726492d3ae506a83a844492aab2326759e89f640ad3e6a5700fc4f0159e16769b5cb2c5998d51279be3d)
          (y
           0x1207aa2499b2837522405d5347c47144267accc135515ffd7eadaae4cccb954b85d5ca5a8b924474b424bb7866b93df)
          (z
           0x58f7787b818b2d559a4dff99a0bac7946ae5ef6f5fa21749aeb4cf2228179effa8d840e38cf169fd77a707645aef9c))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0xb9b16386d614063549a38818cab7275db2791914d026566d7f275d2601e3458e690efbc68425643b4527777b069762)
         (y
          0xecc19ed8c6d36a8e62ad92dee9fae102847eb5ca2b8a3640f5f16a159249f77d839694bb7ea7c1080760577aa2507a)
         (infinity false)))
       (expected
        ((x
          0x128e60d8dc83fe249340152fd830fb6e61083dd994d56d704c6e75ecef8f563d0e655fda1471c8d73fb24d69e16540a)
         (y
          0xd93072cf50b847bea5c8fe9e86839e93db2ee50447730160c29eb79c1ba8846f09f38b152e2a747319ba33e42c8d9e)
         (infinity false))))
      ((input
        (((x
           0x3d0de5b601b9f8773042f687d992ead104090b894e236020c824a8874393c2a30b60a0b22f56a2c56c6695c52d2058)
          (y
           0x15c6098fd4c8ba1e809be410326714a9f20102ac22a2f1ac6bc2cf956bacf7b271a965ea5febe7b506d99dce14bfe2a)
          (z 0x1))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0xa64dcd236d81a5de48a323ec22748bff647987aa1014a54747f5ea84621dcd92300b1dec28c66c65a56cb5b05becd3)
         (y
          0x3be09be6f50adaee0ff97d8f7dbc607b5579ce60e286c049d0aabbaf51ac102d75d0a12cf9a66c2b360e2d4f195eac)
         (infinity false)))
       (expected
        ((x
          0x40633bf43e6592ba7b5552190e8b99725e01b4c6b6ffdab215968c4a83ab1e1bb56efca217d59930e1cd547e1f6319)
         (y
          0xa16d0b3680020157da2d692f573ed6e4b67779005eb121737adaa4a6fb9e708f395e35ff9a9f5770cb7eddb9a9a9bf)
         (infinity false))))
      ((input
        (((x
           0x13c7f3e7ff08ad9ba8e275c14ac7788ccbdd1e5767e872cc86f77bb132f6b2dc0a3fff9cbe15770464b1271d0c56150)
          (y
           0x1a33f00fef7b38070c9e7d24b498a6c65a35f1806143d8e1b1317acd0ec801ba762d460c6021b4ee11d4659938d6990)
          (z
           0xa2bea031ffc87ef3741c1eeb24adf82d7906f7764763bd5b6816d1ed5fc15eaa7f08b787029af726096f7e75419c6e))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0x183ace63e6e0ff3897a9a960e63643567acb0ffa9d9b742452b040ff002ecf8957dd64fa699366bbc93cb6773dbf304)
         (y
          0x1d220fdf258cd3c24cad530b3f0d0d85bbd3737b5471997cecf00a4f2432d2643fb7afd9f1246a30fff0ccb693299c)
         (infinity false)))
       (expected
        ((x
          0x41482b8d82550fb8f2483583a12518ea6f54fa75d2496e732b32e511f4a6067035a91006ec3b7c940db578eac1ed59)
         (y
          0x1ffea1f969d986e7f1e1e8a4fe4d502aecd24de489609addc78b96276689e0390f554e3e857d705f263e082e45feb7)
         (infinity false))))
      ((input
        (((x
           0x12c3eef91cb5b18b6b58ee454601e4f769169d15cae5a3c7761f3907e278bfc2abb0abb534374e211ec2fbd1b855ed7)
          (y
           0xc78d79ae9ff6e6a7e8b2c161e1c45b81b6fc1aebbf4a1d7d2b07ed919d06e49bc751db4630b5d3228c7b62aced98c0)
          (z
           0x15feeb6d96e57fc054c0f5c190df5c30c4f69b63d7e4be246e5c1696b441303c8a4ad244be76c266aed35de0e1559cc))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0xde15b3bda0ac784230aaf7bc497f386ec70c9534896964a7fc8e4ab6b24104c2a5d26e79ddc55efb81b6be57e48b3d)
         (y
          0xcd55a9f5d65f159b835b369dd8678f87132f21eeca7751cc1ca90a2d9bb370bba79da05ebb8814a5eae32741140271)
         (infinity false)))
       (expected
        ((x
          0x14d7be6bfb06ffcd97327f24739d6d851356b7231d8c69dc666195ad020994b70cc7214f48f0459aed9132d7a7e560)
         (y
          0x17343e4a7dd1a31e0cec947eb3b6ede0d864b3835af03903df24f64648bee9992e9eae9bca56f259136e9cd7bbe9d37)
         (infinity false))))
      ((input
        (((x
           0xa7e56d2a73c563e13b0ed911f7f321f9199cd7307bef219fafc76a1712c264fbf8ab94ce0a12d2f0d53f133368b451)
          (y
           0x19581ee36671ec94e49fe55b8f5d1fc2712307eecb755628b390d8683a3f2f4ff5d86226eb5b9edc91e9cd36703b82b)
          (z
           0xac333efe1f02e4d746126786197f61bac824fe0469241a3cf6bec90c059a88e09527fc3ca2f076832cc731d3208d8e))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0xf128ce6788b525a5f6159874fa7df5d3032180de04265d583c10505a29eb188339be4f54d0238727289290b0c4ec6a)
         (y
          0x13498425e2a60ed89dea4f6dcf48cef4af359e90add6cbf8a49ac951f427cd43e90ea27f9e98a0e2f87ba4c0c6ecf58)
         (infinity false)))
       (expected
        ((x
          0x8c0b905e1b83b65e4934c8b07017780ffe708c214b7e75752c06fc2fcc7ec632eeec7ac473f4a3f9f34b782216fc12)
         (y
          0x1608c8b127e0cb53ce5295e730ecfadebfc385c62787518c2d341a92f3f8cdcc7939e940f18c5fca14dae9b05d9f64f)
         (infinity false))))
      ((input
        (((x
           0x16905ac9fecd2c54dc758ce0d1d68922e6b0006f7171abdf44ae5f37effbee450022f92a944a52c2a9a019550e5c9b7)
          (y
           0x433eda1fec0d7a775a7df9557187ca6665a33aef50c6099597da13f6642cf306e114daa931248740b4e741973360e8)
          (z
           0x1ae3a4617c510eac63b05c06ca1493b1a22d9f300f5138f1ef3622fba094800170b5d44300000008508c00000000000))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0x39637e22c28ae4f31af300d26b012564a803c26c6450d2e0a4c90c8dcf91347d73f3cd0cc629a674f78afc75d459f)
         (y
          0x1480b87aa5aac57ac49d0735f5fad996734d57cb8e1afe1a4c6993bf90287921536fa31aa65ec296f3e1fcfbab15cd6)
         (infinity false)))
       (expected
        ((x
          0x163f4443aa7e2f96659a3adf4f18dad0605a296832472eda9466dd066d49d8d29bc677492eeba1ff396c13a019813c8)
         (y
          0x131c2915424a5d4faa584b58ab2fdc0e0ba66a2c1e93d51710545878f2c825bdf1379c71f8e4ac063f35ea52abfe877)
         (infinity false))))
      ((input
        (((x
           0x27d37ba52f86fd9069d52e975fd228e0a7c1e2888833f8c3ff70bef7224cc7ba984238b5cd828c008f44c934875cb7)
          (y
           0x15348fbb2f14e2bf1f2171431a1d18d07338ad310585d63adafcdc8c61f41d606fe207039ef4e8df57dd12d702912e0)
          (z
           0xa4d45b1c786c825c286dd4f39e71815788be24e8a86282d1b8ff4b6c5d2d5d59246b3293e3371d06dedf02186a8b36))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0x47f68932c5f239226808ddabee4b8cb854b9e2a740a84cb9f393fdeee9bbd8b385f8be45ff4b3134ef8e90a8b8e13e)
         (y
          0xc5c875d3c341476daacb8e36ce3cba5feb3054022e81b77319ac17e7be2b27c4443e9f257e4f96c5d055659a569dcc)
         (infinity false)))
       (expected
        ((x
          0x16db75f0347b066abb4bf0b69a8a1c1242b0f5ec0185cb393e251fa3253af81deae6a38b657749fb396eeb287e58d05)
         (y
          0xcd39dba110bf47434fa9015214f4ddcda7c0d18c3897f5265bf947c2237c06296e0aaf1aeb2d0ba1fe76d7f957ff2c)
         (infinity false))))
      ((input
        (((x
           0x2530f19ccf882778ffa634a532538682ab289d85596ccc8f481b666789636be0fcf41d5c140f331ccac0dabd1fad88)
          (y
           0x48fd6a09feabd78a14b56d4e730bda12c839889cbf11c800aa4aed4ab7056b5eec5f3d0bdd3f9895be2fb8c9c953db)
          (z
           0x10f51a8f868e463bcd9fdf586188864aeec132963b56eb1a3b78c4a50f868742c5b3695622aaef9593f0c29236fece7))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0x1139fa57d25aa0c82f6e5c3a749fa924186673e558c47dca2fb9cfff6c68194a14c1b35c74898c4aef7a0c9a4d52d29)
         (y
          0xc94bf6834b6fd14d84323a2aaafa58421f6d7c2bbbcf5b1acd0ff4924e340d3ac362e7388fc596d667a9b27ea9c147)
         (infinity false)))
       (expected
        ((x
          0x8e8c4cfd1d74a6c6fd8ae52612843f44144e79e30681b283d6d715d337ed1a914d839d1c1e23bebc47cca768312054)
         (y
          0x11749015fa6100fb7e9d0195eb4241cc05213a70cd2fbda4100d148a3402a1a17eebb89a594653d09d983f47b0c9a66)
         (infinity false))))
      ((input
        (((x
           0x15e076d1bf9354e00292da3c9e0d06f75e72112e68c71b4005392d32173d33230f973ce8f8fdddeaa7710ead7e0abd4)
          (y
           0x510903717af4003468a329868fa9fbdb6bbf5060c399ea9fa9bcfa57d96ce3fd5ef4f628658c5260c6195c355297cb)
          (z
           0x101d3a5eef9471c8ee1542d782091c619b13ef5dd0f8d8a4a77d4a3ff2a433c835baacfcd256363c638e7946587d0c9))
         ((x
           0x8848defe740a67c8fc6225bf87ff5485951e2caa9d41bb188282c8bd37cb5cd5481512ffcd394eeab9b16eb21be9ef)
          (y
           0x1914a69c5102eff1f674f5d30afeec4bd7fb348ca3e52d96d182ad44fb82305c2fe3d3634a9591afd82de55559c8ea6))))
       (obtained
        ((x
          0x483c75015b6a91c6a1121eff60b60ebf1143e3a59829f1b5b430e9806d77096079c05cf546d3e28e13862a93ee3883)
         (y
          0xa5991615ac16ad7b8c5331be1fe7bb7970e3cd4371ee2c863d02a46d8ee617dedb291a0e820176aaa046981f2be7dd)
         (infinity false)))
       (expected
        ((x
          0xb75b7f7f9bce16215ff7fc92d85d8ba33598a4d2d12a53be4d72bfce0f8ccd9c0ad4a11f149510c340ed53862ffa07)
         (y
          0xd5ffaababbb9c66b896cde593ed337485b268c23b39b2f2376c16604b19a61f9fd44af89d8299a54d92ae61a66001b)
         (infinity false)))))) |}]
;;
