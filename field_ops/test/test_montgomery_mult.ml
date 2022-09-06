open! Core
open! Hardcaml
open! Field_ops_lib

module Montgomery_mult377 = Montgomery_mult.With_interface (struct
  let bits = 377
end)

module Sim = Cyclesim.With_interface (Montgomery_mult377.I) (Montgomery_mult377.O)

let create_sim ~p ~config =
  let scope = Scope.create ~flatten_design:true () in
  Sim.create
    ~config:Cyclesim.Config.trace_all
    (Montgomery_mult377.create ~p ~config scope)
;;

type test_cases =
  { x : Z.t
  ; y : Z.t
  }

let debug = false

(* Software model of the REDC algorithm. *)
let compute_software_model ~logr ~p x y =
  let r = Z.(one lsl logr) in
  let p' =
    let { Extended_euclidean.coef_x = _; coef_y; gcd } =
      Extended_euclidean.extended_euclidean ~x:r ~y:p
    in
    assert (Z.equal gcd Z.one);
    Z.(neg coef_y mod r)
  in
  let p' = if Z.(lt p' zero) then Z.(p' + r) else p' in
  (* p'p = -1 mod r. Since we are using Zarith, having signed integers here
   * is fine. 
   *)
  assert (Z.(equal (p * p' mod r) Z.(r - one)));
  let xy = Z.(x * y) in
  if debug then Stdio.printf !"expected[xy] = %{Sexp}\n" (Utils.sexp_of_z xy);
  let m = Z.(((xy mod r * p') + r) mod r) in
  if debug then Stdio.printf !"expected[m] = %{Sexp}\n" (Utils.sexp_of_z m);
  let mp = Z.(m * p) in
  if debug then Stdio.printf !"expected[mp] = %{Sexp}\n" (Utils.sexp_of_z mp);
  let xy_plus_mp = Z.(xy + mp) in
  if debug then Stdio.printf !"expected[x + mp] = %{Sexp}\n" (Utils.sexp_of_z xy_plus_mp);
  let t = Z.(xy_plus_mp / r) in
  if debug then Stdio.printf !"expected[t] = %{Sexp}\n" (Utils.sexp_of_z t);
  let result = Z.(t mod p) in
  if Z.lt result Z.zero then Z.(result + p) else result
;;

(* The expected result of xyr' mod p *)
let compute_expected ~logr ~p x y =
  let r = Z.(one lsl logr) in
  let r' =
    let { Extended_euclidean.coef_x; coef_y = _; gcd } =
      Extended_euclidean.extended_euclidean ~x:r ~y:p
    in
    assert (Z.equal Z.one gcd);
    if Z.lt coef_x Z.zero then Z.( + ) coef_x p else coef_x
  in
  Z.(x * y * r' mod p)
;;

let p = Ark_bls12_377_g1.modulus ()
let random_bigint () = Utils.random_z ~lo_incl:Z.zero ~hi_incl:Z.(p - one)

let%expect_test _ =
  let config =
    { Montgomery_mult.Config.multiplier_config =
        `Multiplier Test_karatsuba_ofman_mult.config_four_stages
    ; montgomery_reduction_config = Montgomery_reduction.Config.for_bls12_377
    }
  in
  let test_cases =
    let hand_crafted =
      [ { x = Z.of_string "1"; y = Z.of_string "1" }
      ; { x = Z.(p - one); y = Z.(p - one) }
      ]
    in
    let auto_generated =
      List.init 50 ~f:(fun _ -> { x = random_bigint (); y = random_bigint () })
    in
    auto_generated @ hand_crafted
  in
  let sim = create_sim ~p ~config in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  if debug
  then
    Stdio.print_s
      ([%sexp_of: string list]
         (List.sort
            ~compare:String.compare
            (List.map ~f:fst (Cyclesim.internal_ports sim))));
  [%expect {| |}];
  let internal_ports = Cyclesim.internal_ports sim in
  let find_internal_port s =
    List.Assoc.find_exn internal_ports ~equal:[%equal: string] s
  in
  let find_and_print_port s =
    Stdio.printf
      !"%s = %{Sexp}\n"
      s
      (Utils.sexp_of_z (Bits.to_z ~signedness:Unsigned !(find_internal_port s)))
  in
  let is_internal_port_vdd name = Bits.is_vdd !(find_internal_port name) in
  let results = Queue.create () in
  let cycle () =
    Cyclesim.cycle sim;
    if Bits.is_vdd !(outputs.valid)
    then Queue.enqueue results (Bits.to_z ~signedness:Unsigned !(outputs.z));
    (* Debugging outputs ... *)
    if debug
    then (
      if is_internal_port_vdd "stage1$valid" then find_and_print_port "stage1$xy";
      if is_internal_port_vdd "stage2$valid"
      then (
        find_and_print_port "stage2$m";
        find_and_print_port "stage2$xy");
      if is_internal_port_vdd "stage3$valid"
      then (
        find_and_print_port "stage3$mp";
        find_and_print_port "stage3$xy");
      if is_internal_port_vdd "stage4$valid"
      then (
        find_and_print_port "stage4$t";
        find_and_print_port "stage4$xy_plus_mp");
      if is_internal_port_vdd "stage5$valid" then find_and_print_port "stage5$result")
  in
  inputs.enable := Bits.vdd;
  List.iter test_cases ~f:(fun { x; y } ->
      inputs.x := Bits.of_z ~width:Montgomery_mult377.bits x;
      inputs.y := Bits.of_z ~width:Montgomery_mult377.bits y;
      inputs.valid := Bits.vdd;
      cycle ());
  inputs.valid := Bits.gnd;
  for _ = 1 to Montgomery_mult.Config.latency config do
    cycle ()
  done;
  cycle ();
  List.map2_exn test_cases (Queue.to_list results) ~f:(fun { x; y } obtained ->
      let expected = compute_expected ~p ~logr:Montgomery_mult377.bits x y in
      if Z.equal obtained expected
      then Ok ()
      else (
        let software_model =
          compute_software_model ~p ~logr:Montgomery_mult377.bits x y
        in
        Or_error.error_s
          [%message
            "Test case failed!"
              (x : Utils.z)
              (y : Utils.z)
              (obtained : Utils.z)
              (expected : Utils.z)
              (software_model : Utils.z)]))
  |> Or_error.combine_errors_unit
  |> [%sexp_of: unit Or_error.t]
  |> Stdio.print_s;
  [%expect {|
    (Error
     (("Test case failed!"
       (x
        0x13131db815094d15f9c2ce10a2dbde40cd206eeed2068e054d139e3295deb80c32be4fa96eedbb0ea244e2fd0640861)
       (y
        0xf1cfb6c4f9ec58f2e3303579dc38a337217aa49ea8dc5d3529bf2f2922c2c62154d684eabdf050ab2d243a4de48ce1)
       (obtained
        0x18a7a73e837a4b75d81e2aefc542fcb0314e88bc18fbd35c8a8e723a328ef2a672d9cb93f56d278fc5c860eb7bb04df)
       (expected
        0x14a56ca03f040c1eb42942b0f2214f27938fdf7a7673d2863029ccf072fcd7ce544afdc38033f1e0442c6a470f31013)
       (software_model
        0x14a56ca03f040c1eb42942b0f2214f27938fdf7a7673d2863029ccf072fcd7ce544afdc38033f1e0442c6a470f31013))
      ("Test case failed!"
       (x
        0xc1c16e4a8d949226528103c9a0d6a1bdcd2055655ad6777ed3ac13a4cf441eab17a965f37338286b476e11b8fa7dc6)
       (y
        0x20ced86e9a01a4726127b83241d6394b4c84ea15a96be4be46ff0cedfe7bd4e62efe21d2c2ce8232a42c5321cd10bd)
       (obtained
        0x1787e05f35be98a7dde5d1b7312e25753e14f7af157720038381047a68d469e32327a7da56086fac1c4aba185ccd7d9)
       (expected
        0x48c6d08a38aca6ed8ec5d8be637d7ec199639a8fa893e50540ef9f6b9adcb246d315b46f37298bd7778749623d6914)
       (software_model
        0x48c6d08a38aca6ed8ec5d8be637d7ec199639a8fa893e50540ef9f6b9adcb246d315b46f37298bd7778749623d6914))
      ("Test case failed!"
       (x
        0x2a28947412ef48f389fe8699002392d7f259b841403ff7aafde7ae80c839227192f1e065e551e01a0854636b30817f)
       (y
        0x162bc44b59541803f76221bbe1b0b051a41d615b36fc29bf843ae343000f45a6194e68de5479f9db353a711aead0987)
       (obtained
        0x1ae15e180340ec0c9b4f69954bde380f136d1a64266ad0338a9bf9e7d295afcb744d50faf277384679b49c71270a682)
       (expected
        0x4be94642e9bed8dc3e8d9672b96c88248f7d6cb419ee1e22277c0d2435a497899f38373e048183999a7920a266bb7a)
       (software_model
        0x4be94642e9bed8dc3e8d9672b96c88248f7d6cb419ee1e22277c0d2435a497899f38373e048183999a7920a266bb7a))
      ("Test case failed!"
       (x
        0xa70ade9074e931a6e78bcf07f55ed14afc4808b168643633f2c2e2721c9e9c58c4d8f53e5eaab5ba9612fe51e8c19)
       (y
        0xaf7722ef91913921f470348629ce166be4787262a25432180219756138df944dbdb13065e06fb7f67ef912dd4a4c1a)
       (obtained
        0x2ba35936f185d0afd1ac26c0ab5f6b2f3d327db7674d0ca949e22af27eb9c3b418629cfc161217003f976657b5eb3)
       (expected
        0x183f2a4eb44351e307c7df817fca280f81094688166b002623ea1252ea3bb30d2281599ab505f9a1655b6b3bb2dc8b8)
       (software_model
        0x183f2a4eb44351e307c7df817fca280f81094688166b002623ea1252ea3bb30d2281599ab505f9a1655b6b3bb2dc8b8))
      ("Test case failed!"
       (x
        0x90a1347acb3dc506c8834470858d77951179680f77bd9dadf9940666934d196aed5cc0a888200351049af71bf3694)
       (y
        0xf16dabb600b8143cc314f619f4a5c9fbff20d79c1cee8bb98aee9e76c250a602be4b002801e9b7e70c0b35af2ef1d1)
       (obtained
        0x19e38728287b2784788381a790a2124b54eeb535a42292e3ef5e0736b8f015c292a77cac9650743d17e0cc30cd40c9c)
       (expected
        0x1a27a4524a15975c146be64767f83b1eed50600f94c6e288be5136c9f5d88f60c3571bbb18943227750950b5ce0524b)
       (software_model
        0x1a27a4524a15975c146be64767f83b1eed50600f94c6e288be5136c9f5d88f60c3571bbb18943227750950b5ce0524b))
      ("Test case failed!"
       (x
        0x1226cbc22cb5cba2ad9bd5b0a72f0bb8cd39fc73f78362e4aa58d598fac45d6b58c60eed8ecd0c9d9b572aabf9d3a32)
       (y
        0x16866bf294e1e255d39f64246f937e988f83edcd6e456214e62cc0a98f91ca1b537c6019b09da0580bc84e6907baede)
       (obtained
        0x1561cc4ec36d779c8e9dcd03e241753e6406597be5f883445df4c1c8c2a782952f9a50799d8f0b48c955e54c33f2319)
       (expected
        0x6c8999eaadd44d228d87dba9172fbe74f8140f95e161194f83cb6694235ff3600dd3e72edae0ee4daafa8d26ca88f0)
       (software_model
        0x6c8999eaadd44d228d87dba9172fbe74f8140f95e161194f83cb6694235ff3600dd3e72edae0ee4daafa8d26ca88f0))
      ("Test case failed!"
       (x
        0x13636ef6b475ec2396d82542c28867692d3c24441b1bb8ad958548ffc857ddc110efb8e7448fce5267d789cbb6c78fe)
       (y
        0xa38b2100ae64547541e9337a10d67532b540d1365935e16cd4d16ed201d37ecb89ab08efb4eea9904bbd678ffe4122)
       (obtained
        0xdfebf4d48fa53fd21a789c751566503ad8f409f0a6129bb71c34c4b0d9b541d79f754aa979dadb26d743e719a6ba20)
       (expected
        0x896eb5428ddf96e1eaa51a51f3b8db33d530bb2e1a43f33257313755fb70c49db1b06fcbc216ce160b5515b7004711)
       (software_model
        0x896eb5428ddf96e1eaa51a51f3b8db33d530bb2e1a43f33257313755fb70c49db1b06fcbc216ce160b5515b7004711))
      ("Test case failed!"
       (x
        0x82f91f3dd4745836c54b6326f1166e21af6ad0b8ebcdf22314573d69182062a21e6a55eee842a717f0ac268bcc32f1)
       (y
        0x32dd150d6398b7fe78ac6c5afb80c319fbadf39c933ab432c9ee926ed55cdaeed93d6e8a051523568b9461f138a39f)
       (obtained
        0x9618d77808f75cd790b0aa8459871def396fef15af254423b3c48c19364028e250f133234f6121eb35242f31e51453)
       (expected
        0x19911391edfd0f3d0878638df31e35f0a77d8082fd73766254d2bd776fe8ae2679f2da51ea4ce0a49483a53b3f4d0f0)
       (software_model
        0x19911391edfd0f3d0878638df31e35f0a77d8082fd73766254d2bd776fe8ae2679f2da51ea4ce0a49483a53b3f4d0f0))
      ("Test case failed!"
       (x
        0x5a506e964e84b3de2af2f289100f6661b46a7d7511c876fffe6531d5c3da18d49d0365fe4986d6ac51e15a84b1754)
       (y
        0xc048668ac6bdde0b85665a9e3dc5800bbb5c78cfda49a0fbdcd5e83d915579ba3426de639e22c4d0bb9c0ce3a497ef)
       (obtained
        0x1560ca6cdff9907149800ad641f982c521732f367b0876cc2c98e94ac247d956733790e1d7572150e4c802a9fe298ed)
       (expected
        0xb6f393bc3311c7ee2d7e909d8252226809c7cf4451807b9120b44974da6a14b3beeb27d7c2288592371706d2e1f450)
       (software_model
        0xb6f393bc3311c7ee2d7e909d8252226809c7cf4451807b9120b44974da6a14b3beeb27d7c2288592371706d2e1f450))
      ("Test case failed!"
       (x
        0xf244e7a650dbe51ed53e1cc396ba9204a7592b944b3ce3e2ea8bc038688dd7a0e3bbd736360b212858b8fbf5af166d)
       (y
        0x14b5596bba55f697ef9ac8fc1d73b12713894615f9b36f81f023926ad94d79174304c267630d2b174a2881fbde373e7)
       (obtained
        0x14f246ea81085d89bbec6f65a9345085d09b419a93c6d82519f0ca300174db7afa0a925a39ec193431994973cd43bb7)
       (expected
        0x14921c0cf4d57137653b6b1f76570fee9ab2be0cccf24098459371ef239688a3505bf83b0bcaf9a34fc6bac5ca23ec5)
       (software_model
        0x14921c0cf4d57137653b6b1f76570fee9ab2be0cccf24098459371ef239688a3505bf83b0bcaf9a34fc6bac5ca23ec5))
      ("Test case failed!"
       (x
        0x3c8edbc96334924dd32688217de4ca2613bf1a49ba39145659a8b7bbc4b930a60ed5712eebc737bc9ff6d061b625a)
       (y
        0x582e5817a8b2c8ee6d0c55015352129bd179e73f069b633e2e072f52d5f1377b4de5924f3f9d9cce09e6402c3febcf)
       (obtained
        0x14d82654199354d2e541d472366de640fbf623763a2f9737a7c66f8afc1b2bc3dadd1e5cc5056cc569910aef6a135f3)
       (expected
        0xd664a601de508621244915146b80190e5ac5a9ef0b4b30d0b96763da3b55777dc8bb45aa0c4373d75dfb5c2cb86732)
       (software_model
        0xd664a601de508621244915146b80190e5ac5a9ef0b4b30d0b96763da3b55777dc8bb45aa0c4373d75dfb5c2cb86732))
      ("Test case failed!"
       (x
        0x9e830e7950ebcf9865f96df3d130e59ad6bbf6c7491aaa6fac90b3f0760dcecd1b420d23ccd960ee58c33ee6d95ec4)
       (y
        0x54690a31f5e5c6e0a13bd423884e7e39cef06c4f36a3c73041074040d7e5b2f0711a95cecbf72a327b592bdc4b1a2c)
       (obtained
        0x141ebd400de6d1aa0a21b027b8621366744832fb401cc538b2695c51980a57f31812c7a8a0a997c40a4169fe64ec290)
       (expected
        0x8cc7fe5487fb7c4ef6ff093817d707dd33a1dcf3b8abaf28a61e9bfd1234b7f5fa3688d80d4acde1766d3fbbfeb5b4)
       (software_model
        0x8cc7fe5487fb7c4ef6ff093817d707dd33a1dcf3b8abaf28a61e9bfd1234b7f5fa3688d80d4acde1766d3fbbfeb5b4))
      ("Test case failed!"
       (x
        0x13683823bec7693bc872c2a6e46cc9eb49503be4bdc4947641211d86c5bcd8bbce767e8782c2b93fbd533db694c347d)
       (y
        0x613db0ddaa6af67a02d25876b58fc821d1f2d65fd2fdd3aa55f4b06d8b9cf4ced974fc789d6008b3d2878e25693585)
       (obtained
        0xe0750b9adc7553e6680f0d0578c558863ac141e8956aed8862a60b891ef907e70636d14dd8d6f90072dbd88bca560d)
       (expected
        0x674eb44f1f7fedb40f63346dc773a372eeac9063a8df93fceb65bf4848744f80350dd21d191610e63d8290bbc85994)
       (software_model
        0x674eb44f1f7fedb40f63346dc773a372eeac9063a8df93fceb65bf4848744f80350dd21d191610e63d8290bbc85994))
      ("Test case failed!"
       (x
        0x131c8930c045809a0b1ff5a2b63181b4c41c23f0c08f51f854bcf663d893eadb310fbe3cd2adecd1f41dc0cd95cefbd)
       (y
        0x9f0e5aa738a5d750d7056d2981aad0ddf172afadee1c33c684f2934a816afab30e5a5f4052ed8bc81284b62aac933d)
       (obtained
        0x4b5ad50ea24fde30c878929b5db69b48344acb03a0e96fa5720584ce0e6e98bb1aff0691413f36b37b568fcddc6a4c)
       (expected
        0x5764fb56578f3c0d91475abd03eb7725dfd7246a1ec9eb159eff3809db0d4d70fdcac663e8d60bd8c65251db660a9c)
       (software_model
        0x5764fb56578f3c0d91475abd03eb7725dfd7246a1ec9eb159eff3809db0d4d70fdcac663e8d60bd8c65251db660a9c))
      ("Test case failed!"
       (x
        0x104195538d8adcb011d41f42cdd34ead114676cb3f04cba29ee846e7bbbd0f0e5e56558cbc248e2a35782b98dc84263)
       (y
        0x1a24c87fe1b8a4750397574c148b3b5699839b0b6b811cb4ad326ff39c3bb8cbc7f762135689d1b303736d4ea0521c2)
       (obtained
        0x10392b0eb970240f7586712d222edec0df758ff23811fccf2608b7de7aaf1e0ec5b26ab7bbae9a8b73190771ac39cf0)
       (expected
        0xd48579b02644f672ce7623385aa876daf3d86fb0a32689ada6d3459f7a852682265c3a0d0d52b24e2e9d6486edea1d)
       (software_model
        0xd48579b02644f672ce7623385aa876daf3d86fb0a32689ada6d3459f7a852682265c3a0d0d52b24e2e9d6486edea1d))
      ("Test case failed!"
       (x
        0x11274cd3d25f1db9578a7e05a18adb0106dee277901ef057959e26055a0f970b4475907cb020e01a246dcc3cdcc1d9b)
       (y
        0x70891110a03628da82ca52a82a6c22329960085be0c5add0851d8437111e8b488201eee85e62066e788368926d6771)
       (obtained
        0x2119c4becb32efabf423151c985f802970abc7e22e44726ec5bba424d20357b41347e8f9c728e0ced79408c4f7a0f3)
       (expected
        0x666c98fdc5c9dc5bb57b51c2bc9a29bed0e4f04b713a52326459be536e1e683a4522b244e6bc42c0c6f9ce70bc451e)
       (software_model
        0x666c98fdc5c9dc5bb57b51c2bc9a29bed0e4f04b713a52326459be536e1e683a4522b244e6bc42c0c6f9ce70bc451e))
      ("Test case failed!"
       (x
        0xf53bad7b79868088ca64d81871a87fa712ad1faf609c7a39a7f52095c323f7490c73f3bb7de00b8c7377ff0b592579)
       (y
        0x6c8854da1e36b87f2ae5527e5c948776c12fd981f69a884791f09363570470229c259a9a21985d46bd8875a9509c73)
       (obtained
        0x15ed8b8df1313a748edabdcc3ab3694115242748d77dfd216f05b2c2d7a2488946315141dc9748971cca44191bef452)
       (expected
        0x3637f351eddec5e45227f8c6bf26cbadf3eae287434f07158c19914a2f6e709c22e7a70b73bcb9ed02fdf6e5faa5c7)
       (software_model
        0x3637f351eddec5e45227f8c6bf26cbadf3eae287434f07158c19914a2f6e709c22e7a70b73bcb9ed02fdf6e5faa5c7))
      ("Test case failed!"
       (x
        0x1851ed5c89cc26814cc0050fbef81ed3916fca808a168a8992cfc1418a9e6982feece327eb9dfcd53ac0bcaa54260cc)
       (y
        0x946424ec816f6cf4ad7ca69581e160da3c9fe3f8327de343c467d2f76b5f37c224635435eb2b6a4540da55f9f90d1b)
       (obtained
        0x38fd0a16167f930526cbeba0240d3caf64b73047c6e1e645edfe4dddfe7c850477f53d031db1af0151319a87e65de9)
       (expected
        0x139403e62b14c73cf81ac8aed2a2671237c7fa3933be98b70bd2bd1364ddcc13918247e06236ce851de81b5bc304ebc)
       (software_model
        0x139403e62b14c73cf81ac8aed2a2671237c7fa3933be98b70bd2bd1364ddcc13918247e06236ce851de81b5bc304ebc))
      ("Test case failed!"
       (x
        0xfead72bff3973fec03c633abaec30bd7b5d913fbd4bcf66fd8ddffe00b2f3d4bd9f621608325c6d578e58ef52d9e7)
       (y
        0x1a6ed07809c3dd033003c542f8d0c050a7991648c31f176f9b5b475f3607542569c17247f401870a4ff7101fd89cc9)
       (obtained
        0x18a410c37f5d41d06f063e58a7e45bd67384a841a061d66d4fe62b972dbc274511ddeb2fb49c85b6ebf0ab0dee8252e)
       (expected
        0x12cc8b1575874f3bee5bc5710b889c7b8787c00375daea7626604967d44123094b6f02ed37793cfd59ae0783e0070bf)
       (software_model
        0x12cc8b1575874f3bee5bc5710b889c7b8787c00375daea7626604967d44123094b6f02ed37793cfd59ae0783e0070bf))
      ("Test case failed!"
       (x
        0x998c5c553aad808c258bfab2aeebf71eb27db3e52fc7c8d7c40c82e00b1593f255fbfa334ef9bd82af14cf3253fdaf)
       (y
        0x343202d1dc0f42e28c8894fe7927dd0dceeea1ad0d912dc9fbe81309e06d58013c109ab0075537556c629bff6da41e)
       (obtained
        0x16c07f6b7e77993431e175acc1ebb4394aebd01a554f69f53a5cd80302e6506dfafef7f1af73ed35ba4e181233b13a1)
       (expected
        0x80022289d0b99a23e10e301766044b252b2b585faff90c569019e63e725f56c9744d00f1a3663d3c9edfa0b33cf5b1)
       (software_model
        0x80022289d0b99a23e10e301766044b252b2b585faff90c569019e63e725f56c9744d00f1a3663d3c9edfa0b33cf5b1))
      ("Test case failed!"
       (x
        0x395fde42ced9e08ffa07296ed09f0e99f6bd914ec229279695ac209ea9106d9e94439f9b63acfdcc65efbd5aad3813)
       (y
        0x1388d81f33ded8e6ead4a019226c60ce3e678525b3d5478669773eafdf1c246745031324391c9b3a6584dce3216b509)
       (obtained
        0x184e9214ee101f7adf678d72ac51923e0cc417a44b4c3373c6a16176b019c84d9beaa7c90b21ae29e8f6a66486bde84)
       (expected
        0x1491d35f3b1a6e0d8008eddfb5946821d76bea6000efe83ee3f200113a852f177cb942c0c6cdee5f2cafe5442f0c31f)
       (software_model
        0x1491d35f3b1a6e0d8008eddfb5946821d76bea6000efe83ee3f200113a852f177cb942c0c6cdee5f2cafe5442f0c31f))
      ("Test case failed!"
       (x
        0x320ebf9859baa302f40e35b9bfb725387dc2b2a45daf306662bbb9a2fc56bc10f8225e4f90b8f5a0a55507d7eb4456)
       (y
        0xfcfc04d7ec0e8a4623e56c3cc6eb5a6e0d3a0731a9df6da7dac7bf1ba82030aac361cb77fd400bfaa6743ac13dff80)
       (obtained
        0xcbee955bf7312c1357d3de9e9d0b8f51c52de823829ad110cde443a1cc3877b8c4a0c4f2f6efd5bb1831e1fb38f6db)
       (expected
        0xf17b8ab49d7de346e38290b924f025b5dc215916131690f23b747ede7aa9208715e756f798dfa4229f436bcf63fb1d)
       (software_model
        0xf17b8ab49d7de346e38290b924f025b5dc215916131690f23b747ede7aa9208715e756f798dfa4229f436bcf63fb1d))
      ("Test case failed!"
       (x
        0x91eb08d2666f26cfbf06cf1020a418d6cb0b89c775342cae260272243d832f713bde09a4213c8db97b20e37ad69cb8)
       (y
        0x1726835b69225ea20313178409612d0a4a9fdd7a9ec27a851c2670d4528fc6419d7a082d0db457ac269e95f3b93d6d4)
       (obtained
        0x11c2a2160e19fb36fb34c299e0d80311e2da277c9cf59c9214029fe77fba280fccb2643b5060059c13b8dd3c0eab2b5)
       (expected
        0x113460c1d73c396582503a5aa1d642076189bc85b87acbf0982f8ff7836c422e25f4f726a0d5365d6b438983c72e208)
       (software_model
        0x113460c1d73c396582503a5aa1d642076189bc85b87acbf0982f8ff7836c422e25f4f726a0d5365d6b438983c72e208))
      ("Test case failed!"
       (x
        0x805501ecbd4ffed73f88ab1985beb6ac52367d43969e522ec52400b36033a7e52aa6b6fb35bab1fe0768a42e797b9d)
       (y
        0x195b19f36a696256be6476479c9f59312e3c284e44d5dca2ff47cf54d33a147c5454d290ba945a481394b8f9967c70d)
       (obtained
        0x6893da076ace4636a6bd4565711667b91bf23f832ddbee90d250e029955a9a8f704dc6b912acfed0425b37aed7bdd2)
       (expected
        0x8e64a16470ff7d912c928470048f01bd71d2b672da52143e9b1d137414df77d9fcc1d4a4ba05dcda416c2a447f8f3)
       (software_model
        0x8e64a16470ff7d912c928470048f01bd71d2b672da52143e9b1d137414df77d9fcc1d4a4ba05dcda416c2a447f8f3))
      ("Test case failed!"
       (x
        0x43bae7120229d57fc7c3a811e44aa04e719f63da26c559ed66eac78c2b855e976a8a65e7be8b064ee725ede5b64ed4)
       (y
        0x11e3cad28d7ca51e34bb6b97616a27cac857a2f575a66ae76dabf44550ee0d8b4cf349669f66359fd7d5ea31142746d)
       (obtained
        0x1106c4ef36d86940af293f094e8ef0e85fd76b669f101ef874dde638bc2b4af2380eefe620c1904074628b6329d098d)
       (expected
        0x6db32a93ab72df8f060abc7aee40d5a054c10652eba51b14b773734145ebe710d377be78a395a9c1386e70a19e2af4)
       (software_model
        0x6db32a93ab72df8f060abc7aee40d5a054c10652eba51b14b773734145ebe710d377be78a395a9c1386e70a19e2af4))
      ("Test case failed!"
       (x
        0x19b21eaf27c833d51cd3496602a3a1b7546819f4864639c2f80ee70c1ac81caaec1cb321e3bfb3acf328108dcd713f)
       (y
        0x2268c375d426369b89d142f1ce7268f6e95344df2f8b457d015bbb4c4078c78b2a7eb22d4bb47bfec1a843f9e23b4a)
       (obtained
        0xe00224ca7e463646212789d5ee861f83e00df16c4350aa51ce53e0816ba9591393e793f6cbadc13b74dd099b194715)
       (expected
        0xde8f12bf42c644cbf5478fdbc1ba7d7b3e889bfc9d26367b24b7f499b60c0658a90b053702c09a77257f77bc0d6c47)
       (software_model
        0xde8f12bf42c644cbf5478fdbc1ba7d7b3e889bfc9d26367b24b7f499b60c0658a90b053702c09a77257f77bc0d6c47))
      ("Test case failed!"
       (x
        0x1ae3a4617c510eac63b05c06ca1493b1a22d9f300f5138f1ef3622fba094800170b5d44300000008508c00000000000)
       (y
        0x15c22a2f66da571430b6802c934950a34495f7a9a1575e3e211df4553eb3357a9e63e9ec21b97e63dd5a8aaa69fcd49)
       (obtained
        0x106f5603a1209b5ae878ed654d1ecb17a83267bf07e4d1f9d3728f76b5b5c2ed1c8da62abf5e3cb9c796f101b7735b7)
       (expected
        0x4817ffa4fa491d8f436deb0abebb931d661234167294c2536461a8c06e61f2129d74c6068a759b5379e5226f06ee1d)
       (software_model
        0x4817ffa4fa491d8f436deb0abebb931d661234167294c2536461a8c06e61f2129d74c6068a759b5379e5226f06ee1d))
      ("Test case failed!"
       (x
        0x12f13383e1492707c241f621617b58406021763d4ff7eeb13cdd0aeb21b7c00de0b5571e50ca357c283323d04cf5ed9)
       (y
        0xcb9b168259a8c5cd6b4d3bcec500ccced28937be0147bf21bc05f7ac8a0c14f8dc8cafffd05fc0c9297504ba9b1939)
       (obtained
        0x147ff1ac7eb28475d48b764dc93c1d6e3550a207a371af9fea0727eb1bffb3b74d6e238305e8ddf3effbda0ee4dfbae)
       (expected
        0x16044b02aed8e376b77c7312922b88e809ace6f71ed299d60d454e2df23b2851a7ac97ea6bd5edf8453a236daa5c070)
       (software_model
        0x16044b02aed8e376b77c7312922b88e809ace6f71ed299d60d454e2df23b2851a7ac97ea6bd5edf8453a236daa5c070))
      ("Test case failed!"
       (x
        0xf3722cf71860efa8addd1ff951c2ccce292354f6d1e3e42bc2bd0261a61617af2b459f57882a6f93f998c418468ad9)
       (y
        0x312d9f4867d6b02f2836c9c44855729820510de609ef09d5c13b2f543c65ad496792223acf2c0b2672ab4e53ed05f1)
       (obtained
        0x576767b061d994a03fc0dd6625b94e28a225eed0737d1ae69c3cc32758c63d9a39d20e115d76cb5cf4eb4cbcefe1f)
       (expected
        0x14b158ed6b147f1770d564c916e223cfaed599ea25d7ccdf3a18ab1fffc125693208ca006b6a7317ffc53f67973ddb0)
       (software_model
        0x14b158ed6b147f1770d564c916e223cfaed599ea25d7ccdf3a18ab1fffc125693208ca006b6a7317ffc53f67973ddb0))
      ("Test case failed!"
       (x
        0x5d137b823b14945ff569c6e2db6c016f3b88de4f3ba69362b9c82d6a6cb7d2b3ff5c0bd0b5869d14fa9c838a0d30c2)
       (y
        0x138429940267beb17c368ba3e66f282924dcdb35426abb7948a9b56480282832b8eef7acae2b352397dfd5a6f08a81e)
       (obtained
        0xa633aedb5a542b33fa96688699144484cd3b8a09940ddea9b772e88900763a1f4dd70bc94403c03d9216dcf315c3d8)
       (expected
        0x107a72d5085d889f9bee8ec212e8513169f7e569844b3e3fce19bf076858dc33603197d8679bfdf823778cd7a6b4cd8)
       (software_model
        0x107a72d5085d889f9bee8ec212e8513169f7e569844b3e3fce19bf076858dc33603197d8679bfdf823778cd7a6b4cd8))
      ("Test case failed!"
       (x
        0xe0e077bbbf74b96b0c5693bc1b8e098c1f8da32de99241967d11e4f07006f08ec8a30fe2a415eff6981aeed6e4cfba)
       (y
        0x6bec66b0fcfbdc91e86dd2910224743e99e42ad25b839ba479120a270db9b8d3d93cb39a47a548daf939a003cd388b)
       (obtained
        0x19abe4e994407512b4dae6e4a0ca57705d6cb28129418cd64c91414c40496b930c1de495de3b6246d965111e7aa9954)
       (expected
        0xab5608f6a2155556a926692c623b80c4b184e7b63965861c5a6347ac820dcd268831091f04725e73e448740b5f0236)
       (software_model
        0xab5608f6a2155556a926692c623b80c4b184e7b63965861c5a6347ac820dcd268831091f04725e73e448740b5f0236))
      ("Test case failed!"
       (x
        0xc99e1c39e7d0532e4ec59c6615b2cbf71356e15f59a835934b6d174cec36d5857c3299dbc77439e24d2fef94dc0707)
       (y
        0x13d1455b1d39dd80be2cc5c5fef90d515a48158d92ad9ed888ec5db498edd3ef5300502a4dbbf62c766526187714715)
       (obtained
        0x18693f5fc13ad3ca5833776e4ac7204b00e881441ba40692be6d05b3fb65b7c8126ad26ae828b43d306694d0c80a503)
       (expected
        0x9550cf15ab93e2ca0888afee379cc64083e8870a2be449604d5482f7031afb0f644813a3e5bc93bbed02bc7043fca9)
       (software_model
        0x9550cf15ab93e2ca0888afee379cc64083e8870a2be449604d5482f7031afb0f644813a3e5bc93bbed02bc7043fca9))
      ("Test case failed!"
       (x
        0x12cd9bcbe8a122bfc5d2c40a73252999ec2e2a1144562a681784d4d57f970991227c758aab1125762d945ee15bbf7fc)
       (y
        0x1941c6758f3e335b640fb326577c7ce13f7e6d836546a4775379659dbf2086d8f58ab2a70116a6b298b789ceb0a9cd7)
       (obtained
        0x19a3995b2227fb2d96701fd7eb4d1936d52682b192485d1e7e73775cfcda6d10d8b43a684d07c6ea299ba5efe4371ec)
       (expected
        0x2374bba6e00148791a6a1e091dc0de8f1fbb51383553a2609364b731f75a7de9dc034e874367b8aa87b85d75767664)
       (software_model
        0x2374bba6e00148791a6a1e091dc0de8f1fbb51383553a2609364b731f75a7de9dc034e874367b8aa87b85d75767664))
      ("Test case failed!"
       (x
        0x1a9e172a9f1d463f1e48aa6c282250dc9249942a7409865133b09fb6d10968982e23efb0eccc78bbab0802e90a6a0ee)
       (y
        0x972dd63b20c172c9a9fe0da26e3c9014c02027ae3292b3e78d943e828b1bbb3a95f8934b215569964234577d60fc6c)
       (obtained
        0x172553ea33750b1c151a40a030c5e18a9e40d007ba15fefb65cecafe97e34a345d7700bfc244c5bc57c15b188e2708)
       (expected
        0xec565e782ccf360bda6aa744de8577fdc243328a01699a5574b8ecf33ecf34438aaa679701289afbfe2139c5a5c3d3)
       (software_model
        0xec565e782ccf360bda6aa744de8577fdc243328a01699a5574b8ecf33ecf34438aaa679701289afbfe2139c5a5c3d3))
      ("Test case failed!"
       (x
        0x271144f31d6bdb150da9a0ca01f9708f6ea7740b08cfb85dbac5a0b16e0ee09f65ff59149863192f1b85ff55027098)
       (y
        0x12b277b111731fb10903fcf2c68c56ff5db41587992295ce5077c63c2d120c585054783bc84bf2c87b1ec5864440b5d)
       (obtained
        0xcb40ff0c6c9a9f7ad85a8d8b664106168afaea171b56af975014f76532f9204a8e541a688f35f91056c3883428605e)
       (expected
        0x125ebb847ffe1f2f97b2f90d3e08bb115486e866984b074f5a448b545cc203a1c8ab645ed2b566a32c8eb9b8c0fa3d)
       (software_model
        0x125ebb847ffe1f2f97b2f90d3e08bb115486e866984b074f5a448b545cc203a1c8ab645ed2b566a32c8eb9b8c0fa3d))
      ("Test case failed!"
       (x
        0xbc8e77ac548375ab7d486a0dbbbb76be5b1e2a64035839152a6e415a8d7b8f28ad5f44f00ba13907cef20efd4df7b5)
       (y
        0xc9bbba207c916768bdd38737cbdd0ed7011b38495db91068bcf16c8c584ad9978ff62b0ba8a50a944180eb8047f5cd)
       (obtained
        0xd8ec88ebedd4fa5d5ca3cb76ff4f77aacd1a4dd4aa7059dd38ea655be2bda0b9a1555cadc2fbbe2df9f08c5ce8610)
       (expected
        0xdf34d56e73b8d5987b4edab605d91cb565107bdc61afd6409b794d1229094ae68ae6f3e1e056d277128612e7b4ce77)
       (software_model
        0xdf34d56e73b8d5987b4edab605d91cb565107bdc61afd6409b794d1229094ae68ae6f3e1e056d277128612e7b4ce77))
      ("Test case failed!"
       (x
        0x51db20d06ca337822e355c21e20f4e2a222afa3a089cf0fac58e3e19b85cd2f67865b86cd81d3b6e52a3e631a935ed)
       (y
        0x3d704f9262e7193d9780f6a44f1fa344f9999709e3974feaaf6ada418603c1120796eda4d0be4b406cdd67b2f6e4da)
       (obtained
        0x6ceb4d8ea4ad08f1b1b3ec6db43e35c9e259367742d7a9048cf26170fa2052e0302ed72a11efa4848ce59f011725a2)
       (expected
        0x1299ee4e5df846831b936606c991be2776c9cce7c7f80e96827a8d7c9a07aa986bf71f4e210abdc634497b712b040d4)
       (software_model
        0x1299ee4e5df846831b936606c991be2776c9cce7c7f80e96827a8d7c9a07aa986bf71f4e210abdc634497b712b040d4))
      ("Test case failed!"
       (x
        0x1307afb2a3576dc8abfce20dfdce73efb58b70ecafd740789a00909e3a44e244f9e84588fa3cb5c0b96a4693c3d8b9)
       (y
        0xf2f7b1386b673e7d6ecd1218de2784c8a16d66f402165fc9169364c25550aceaa741807c14cac9f1443658f650f0d6)
       (obtained
        0x56d49bc76523319f5d10b62cec23befc44f63f10e9230db11e47b9c120e63b5c0a23f6ef06d46e302e11c583248ac7)
       (expected
        0x1a8656d3a27b389b5bbbf7417d5199572a88aafcaf1f8e4cb52afd9c8587100edd8f8b728fc343f7b5f54c15292ef44)
       (software_model
        0x1a8656d3a27b389b5bbbf7417d5199572a88aafcaf1f8e4cb52afd9c8587100edd8f8b728fc343f7b5f54c15292ef44))
      ("Test case failed!"
       (x
        0x4fec513daebe5c80fe0f4374d51c48ddb324b334d93ef4a04c598825a3011abf4bdecabcd604ab4e6d3bb050163bc1)
       (y
        0x10e6c3a8a3e70dbb843fb00cfdb61b6a32503af15243394f93c94fb293b681832ae4c3028a4360dcbf60434cb1df4ad)
       (obtained
        0x195d1ed8eb73f1c4bde2343b9db4f31281973c1d1c99962646fd13e43cfe671235f788bf8c9004ecbc9b352d412cbf5)
       (expected
        0x124f35c7908ca7b9df87a941aeaebc59671d575925a36a0f0fbecc835d803374697f1acaeb07c58f372dbc5b975c251)
       (software_model
        0x124f35c7908ca7b9df87a941aeaebc59671d575925a36a0f0fbecc835d803374697f1acaeb07c58f372dbc5b975c251))
      ("Test case failed!"
       (x
        0x131810aac0f29bba2e822dc9e976927303ae33b1992f9ecad47951e3dee8f846eba090ae5e31c5965ea21edf10c6112)
       (y
        0x4b79ec6bc3841b69d638e6d1b03acffdaca68f67880bff00fe9a794d1ce6411fef5c0bd4736981c023c37e128b5c07)
       (obtained
        0x24e8c013452e26ed0f950676fe902f3ae3b043dbba676cdf6eb9a4d1c5cba3332b0a2e758e8cba0dd53ef312a263e5)
       (expected
        0x103e5b40ae3b62a97476413e123fdaa64e4d7c896276dc6e01a23bcef13aebf21b859cadd4012817f4366ff904be78b)
       (software_model
        0x103e5b40ae3b62a97476413e123fdaa64e4d7c896276dc6e01a23bcef13aebf21b859cadd4012817f4366ff904be78b))
      ("Test case failed!"
       (x
        0xebc68df5736d0aaa8ef992cd44f0e22adfd37de9f1abf15fc39af2cbc0c6428a5aad77f57cde1b86a1d0370e324a3a)
       (y
        0x17dbab375734681c4e3a207b6af8eb60fe250cb60e8a492f81614bf24ff488d751060ee9126135c95d7fef4e0ee08cf)
       (obtained
        0x10a234dfe121322c541a44b6c904cb71309bfd97648eeff03a2670719ce260576b33fb0c46a8e96d57a2c07b7af3f2c)
       (expected
        0x4b4f7eb30374e590d2b5b19ffac1befd7e6658a2c8bdaebb2fd9f72c048bbc65fc9685a9c5df38ec6b93a895266a33)
       (software_model
        0x4b4f7eb30374e590d2b5b19ffac1befd7e6658a2c8bdaebb2fd9f72c048bbc65fc9685a9c5df38ec6b93a895266a33))
      ("Test case failed!"
       (x
        0x2658729102b12a15c2c911dbf4cbfb21705cd53a2bccb63937a905f440c7b3d1334fb308e554373788b0b2bd3490de)
       (y
        0xd7d53bc561ba9332aa5e1c23aaed27dfba86af46761bae6815627a0a4875032a1b9a5a1b6fd45f37acbccadb4c819b)
       (obtained
        0x62cbd8e02e929c52290fd211b19d399bda748cc5842d551d3ee8819c3ed33a49d3af3d957faba363153df45945e8e6)
       (expected
        0x15d09704b3e8b9dbeebb3120d9acb179ae6075893224fd04f5461e63942f82f9205d8813e89e0c8474a30031344e04d)
       (software_model
        0x15d09704b3e8b9dbeebb3120d9acb179ae6075893224fd04f5461e63942f82f9205d8813e89e0c8474a30031344e04d))
      ("Test case failed!"
       (x
        0xaf5b892b006c8031de3540d5ea01f6eb86bf337063fef4f27735f4e0b510661fe0f9af6e6f027214c19f1d4d41a38d)
       (y
        0x4623dd7a6e80bd9da00c7c1e16c78608d2d588d66e4bcbfa30459483ed7b4520422a0c02401d81705c3e03106152c4)
       (obtained
        0xa332431faa001b693d4d4328079f439e6478d0bba7f83baad8965e3225e4005fa4d448d85b1f583168a9275f34da43)
       (expected
        0x19a6d10d9301ffcba5995792b78e8d63d94ecd7906b2e04b82e351acad81ab8a53ed76df08e9d9280bb9d28d18d4b9e)
       (software_model
        0x19a6d10d9301ffcba5995792b78e8d63d94ecd7906b2e04b82e351acad81ab8a53ed76df08e9d9280bb9d28d18d4b9e))
      ("Test case failed!"
       (x
        0x7fbe98c92ed651b60ae0051e30193c8298051f1ecee33b9f2ad7d062f9c4b801424e9fe22b4e64e8772559d87209f4)
       (y
        0x19dee452d44823f20f29c0482c3da82915937413bb6ec3bf0d56557f274345f4af5220d8ce94fccedc7ce3bdd2beab8)
       (obtained
        0x4c7893243bdfb9f5b4013b361b84679e7ae83d9ebe76b3e134556cb3214b0c469ac47396154f0eb4a47481df3a418d)
       (expected
        0x198dbf56414a5a4d209ce59aaae75969a5d29b1546ae8ac49779d8675dfb8c9687176f36a8bafea530efb3ee91db41c)
       (software_model
        0x198dbf56414a5a4d209ce59aaae75969a5d29b1546ae8ac49779d8675dfb8c9687176f36a8bafea530efb3ee91db41c))
      ("Test case failed!"
       (x
        0xbc1cbe23c22ad17a03cca6f2f2e2b57ddb869349b445faf30bddde49732679916eb8d1a4f953f4ce5701e54ee8aa10)
       (y
        0x136e1f980f5301ce017502e70f0dd55cf17c09322a4e97365641d58f34b329a15eeef17d7e25bff1d6ece0b7614fc28)
       (obtained
        0x1c6c2991fcc736a7c1a153a90eaa2d819555474f5546832b5473d188281c797d74fd3218f7af0315f642cd123d0b04)
       (expected
        0x649d1d99dd1ddb7f78943d88ea5c0cb89451ecdd3a2f5fa2159902d703f3b84f0452c8dca467fb264f8731fefdfc7a)
       (software_model
        0x649d1d99dd1ddb7f78943d88ea5c0cb89451ecdd3a2f5fa2159902d703f3b84f0452c8dca467fb264f8731fefdfc7a))
      ("Test case failed!"
       (x
        0x18d52de1fbe304a308a6fb31854a48a83ab3e7460c93f71ff546ff1ca1558932050e057008de9e5e7167ae608fa68b2)
       (y
        0x1abf1a54342c58e9faffd90a6eab8546d0e2e35ad378745fff220a26a48d8544a1299ba16fe324e1fe1d5b9fe72861d)
       (obtained
        0xeda32b9fedec0bcd92c225eaeffa82f6567ecfc36eb8d6e2b0e1035bb60c22352ec215618a84092bbe86c8d9313ea8)
       (expected
        0x1010318639da0427e04c2b2fd6bbfe3ae3daeee24b9d9eeb47553f8b65dd6390f2c6824ae1119a6f3847e2d5d6263c0)
       (software_model
        0x1010318639da0427e04c2b2fd6bbfe3ae3daeee24b9d9eeb47553f8b65dd6390f2c6824ae1119a6f3847e2d5d6263c0))
      ("Test case failed!"
       (x
        0x7bfa9d43ae5178ab9681dc6a6304dd5cbe6c6354b879c182fac08488c25633f2de0c78276577900afed86c8163712e)
       (y
        0x67df46a69a707407d88bfdeea61942acb5d59de1cac2298f33af4519761407754a0a3b8fd1e2ef07b01e6d7787878d)
       (obtained
        0x111643cbc7d280609be33d11556954c2f37d5591fc4fa7fdb27a8171f005fb229523e89314554d4fbb8aa6d35feab82)
       (expected
        0x6e9ea3210c9c99172531f4f08d992db4c31398d1f9700e6c2e586220f06988dd8d6c4d43cab96eee582ffdb5298b24)
       (software_model
        0x6e9ea3210c9c99172531f4f08d992db4c31398d1f9700e6c2e586220f06988dd8d6c4d43cab96eee582ffdb5298b24))
      ("Test case failed!"
       (x
        0x7a38860a48a9ff43fa50063c9152a7e8c6c290150e193acca8915e4d8e01b5efe901db2a408571e84c7b17459ced69)
       (y
        0x257d212de31844c0e1bc37906e36fb64d518c9975c35786b6d4bbec08ade4f5962ef3264a8fae6674263bc50eb858c)
       (obtained
        0x151964b1551d61f0477b54875e915ee5ed8a94de1c4cf1dd200551ec44a52fb3908401a900ee8c35610cadd088ae384)
       (expected
        0x7cda43d187a213f36066b8b779b176d5b6b96a93bba2a3794b38aeb686bad83f6a8c52c2b13a9db1e08f48ff7ad07a)
       (software_model
        0x7cda43d187a213f36066b8b779b176d5b6b96a93bba2a3794b38aeb686bad83f6a8c52c2b13a9db1e08f48ff7ad07a))
      ("Test case failed!"
       (x
        0xe6891da1a1d240056c239d751c18d9192a9833d29ce74b3033a2ce74555df2b77550e952754b03607849481ca0b595)
       (y
        0xe402e12f84ccec3bba6893fae199786e8c34cd32d667f9775c7606df79abefad2ba9ada0a7322fcc962d9493788225)
       (obtained
        0xb43c3f597b1417bc2592722d552a6878f2f34b2cdbeb15e688281bb97e95863291f9366730640cac12b184d2ed81e3)
       (expected
        0x12d50ca909745be978b8e8c7ce2e7b26eacba9e33b0246f63177fb008b23048195223a585cc2f7e238760b2784d44a)
       (software_model
        0x12d50ca909745be978b8e8c7ce2e7b26eacba9e33b0246f63177fb008b23048195223a585cc2f7e238760b2784d44a))
      ("Test case failed!" (x 0x1) (y 0x1)
       (obtained
        0x197a54cc2b694f73c8bc83e6fa79e65f13bd76ad1005c08ad72524fd84e830a1fd0b75f0e22897a6787193b1eba9f31)
       (expected
        0x161de1ee362545634d313ea126f7c08a28b2dce9e80e9a61accd49ca2a491ae83264aa55c1cfac62b0909a28934f3a1)
       (software_model
        0x161de1ee362545634d313ea126f7c08a28b2dce9e80e9a61accd49ca2a491ae83264aa55c1cfac62b0909a28934f3a1))
      ("Test case failed!"
       (x
        0x1ae3a4617c510eac63b05c06ca1493b1a22d9f300f5138f1ef3622fba094800170b5d44300000008508c00000000000)
       (y
        0x1ae3a4617c510eac63b05c06ca1493b1a22d9f300f5138f1ef3622fba094800170b5d44300000008508c00000000000)
       (obtained
        0x9876493d2707761a77b615289c3a2f1f66e4b3a953c2ae676f22e469d4f775b6708c63d31d6316a8495d353a98cf5f)
       (expected
        0x161de1ee362545634d313ea126f7c08a28b2dce9e80e9a61accd49ca2a491ae83264aa55c1cfac62b0909a28934f3a1)
       (software_model
        0x161de1ee362545634d313ea126f7c08a28b2dce9e80e9a61accd49ca2a491ae83264aa55c1cfac62b0909a28934f3a1)))) |}]
;;
