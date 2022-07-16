open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Zprize_msm
module Affine_point = Affine_point
module Jacobian_point = Jacobian_point
module Msm_top = Msm_top.Make (Config.Big)
module Register_interface = Register_interface
module Sim = Cyclesim.With_interface (Msm_top.I) (Msm_top.O)

let create_sim () =
  let scope = Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true () in
  Sim.create
    ~config:Cyclesim.Config.trace_all
    (Msm_top.create ~build_mode:Simulation scope)
;;

let precompute_batch points =
  let num_points = List.length points in
  List.init (1 lsl num_points) ~f:(fun i ->
      match i with
      | 0 -> Ark_bls12_377_g1.create ~x:Z.zero ~y:Z.zero ~infinity:true
      | _ ->
        List.filteri points ~f:(fun j _ -> (1 lsl j) land i <> 0)
        |> List.reduce_exn ~f:Ark_bls12_377_g1.add)
;;

let ( <--. ) dst src = dst := Bits.of_int ~width:(Bits.width !dst) src

let bits_of_bytes ~width src =
  let dst = Bits.zero width in
  Bytes.blito
    ~dst:(Bits.Expert.unsafe_underlying_repr dst)
    ~dst_pos:Bits.Expert.offset_for_data
    ~src
    ~src_len:(Int.min ((width + 7) / 8) (Bytes.length src))
    ();
  dst
;;

let rec cycle_until ?timeout sim f =
  Cyclesim.cycle sim;
  if f ()
  then ()
  else (
    match timeout with
    | Some 0 -> raise_s [%message "Timed out in cycle_until!"]
    | Some timeout -> cycle_until ~timeout:(timeout - 1) sim f
    | None -> cycle_until sim f)
;;

let write_axi_lite ~addr ~data (sim : Sim.t) =
  let outputs_before = Cyclesim.outputs ~clock_edge:Before sim in
  let inputs = Cyclesim.inputs sim in
  inputs.master_to_slave.awaddr <--. addr;
  inputs.master_to_slave.awvalid <--. 1;
  cycle_until sim (fun () -> Bits.is_vdd !(outputs_before.slave_to_master.awready));
  inputs.master_to_slave.awvalid <--. 0;
  inputs.master_to_slave.wdata <--. data;
  inputs.master_to_slave.wvalid <--. 1;
  cycle_until sim (fun () -> Bits.is_vdd !(outputs_before.slave_to_master.wready));
  inputs.master_to_slave.wvalid <--. 0;
  inputs.master_to_slave.bready <--. 1;
  cycle_until sim (fun () -> Bits.is_vdd !(outputs_before.slave_to_master.bvalid));
  inputs.master_to_slave.bready <--. 0
;;

let setup_scalar_stream ~num_tasks ~scalar_num_bits ~scalars ~batch_size =
  let scalars_buffer = Scalar_buffer.of_scalars (List.map ~f:Z.of_int scalars) in
  let dst = Bytes.create (num_tasks * scalar_num_bits * (16 / 8)) in
  let len =
    Setup_stream.msm_setup_scalars_stream
      ~num_scalars:(List.length scalars)
      ~num_scalars_per_batch:batch_size
      ~num_bits:scalar_num_bits
      ~scalars_buffer
      ~dst
  in
  if Bytes.length dst <> len
  then
    raise_s
      [%message
        "scalar stream length mismatch" (len : int) ~allocated:(Bytes.length dst : int)];
  assert (len % 2 = 0);
  dst
;;

let p = Ark_bls12_377_g1.modulus ()
let modulo_inverse x = Snarks_r_fun_test.Utils.modulo_inverse ~p x
let modulo_multiply a b = Z.(a * b mod p)
let c_R = Z.(one lsl log2up p)
let c_R' = modulo_inverse c_R
let transform_to_montgomery a = Z.(a * c_R mod p)
let transform_from_montgomery a = Z.(a * c_R' mod p)

(* TODO: Some very important tests to have are
 *
 * - Use the native axi stream width
 *)

let cycle_for ~n sim =
  for _ = 0 to n - 1 do
    Cyclesim.cycle sim
  done
;;

let test
    ?sleep_for_cycles_until_accepting_result
    ?(clear = true)
    ~wiggle_tvalid
    ~batch_size
    ~scalar_num_bits
    ~scalars
    ~field_points
    (sim : Sim.t)
  =
  assert (List.length scalars = List.length field_points);
  Stdio.print_s [%message (scalars : Int.Hex.t list)];
  let expected =
    List.map2_exn field_points scalars ~f:(fun p by -> Ark_bls12_377_g1.mul ~by p)
    |> List.reduce_exn ~f:Ark_bls12_377_g1.add
  in
  let num_scalars = List.length scalars in
  let num_tasks = (num_scalars + batch_size - 1) / batch_size in
  printf "num scalars/field points: %d\n" num_scalars;
  printf "batch size: %d\n" batch_size;
  printf "num tasks: %d\n" num_tasks;
  let inputs = Cyclesim.inputs sim in
  let outputs_before = Cyclesim.outputs ~clock_edge:Before sim in
  let outputs = Cyclesim.outputs sim in
  if clear
  then (
    inputs.ap_rst_n := Bits.gnd;
    Cyclesim.cycle sim;
    inputs.ap_rst_n := Bits.vdd);
  Cyclesim.cycle sim;
  (* Setup the registers in the core *)
  write_axi_lite
    ~addr:Register_interface.Write.addresses.scalar_num_bits_minus_1
    ~data:(scalar_num_bits - 1)
    sim;
  write_axi_lite
    ~addr:Register_interface.Write.addresses.num_entries_minus_1
    ~data:(num_tasks - 1)
    sim;
  (* Stream the precomputed points into the core. *)
  let precomputed_points =
    List.chunks_of ~length:batch_size field_points |> List.concat_map ~f:precompute_batch
  in
  let precomputed_points =
    precomputed_points
    |> List.map ~f:(fun p ->
           let x = transform_to_montgomery (Ark_bls12_377_g1.x p) in
           let y = transform_to_montgomery (Ark_bls12_377_g1.y p) in
           assert (Z.(gt x minus_one));
           assert (Z.(gt y minus_one));
           Ark_bls12_377_g1.create ~x ~y ~infinity:(Ark_bls12_377_g1.infinity p))
  in
  let precomputed_points_buffer =
    Affine_point_buffer.of_affine_points precomputed_points
  in
  inputs.precomputed_points_wr.tvalid <--. 1;
  inputs.precomputed_points_wr.tstrb <--. -1;
  inputs.precomputed_points_wr.tkeep <--. -1;
  inputs.precomputed_points_wr.tdata <--. 0;
  inputs.precomputed_points_wr.tlast <--. 0;
  Cyclesim.cycle sim;
  let num_cycles = (Bytes.length (precomputed_points_buffer :> bytes) + 31) / 32 in
  for i = 0 to num_cycles - 1 do
    let tdata =
      let precomputed_points_buffer = (precomputed_points_buffer :> bytes) in
      let len = Int.min 32 (Bytes.length precomputed_points_buffer - (i * 32)) in
      bits_of_bytes ~width:256 (Bytes.subo ~pos:(i * 32) ~len precomputed_points_buffer)
    in
    inputs.precomputed_points_wr.tdata := tdata;
    inputs.precomputed_points_wr.tlast <--. if i = num_cycles - 1 then 1 else 0;
    Cyclesim.cycle sim
  done;
  inputs.precomputed_points_wr.tvalid <--. 0;
  (* Now, stream in the scalars ... *)
  let scalar_stream =
    setup_scalar_stream ~num_tasks ~scalar_num_bits ~scalars ~batch_size
  in
  printf "Number of bytes in scalar stream: %d\n" (Bytes.length scalar_stream);
  inputs.precomputed_points_addr.tkeep <--. -1;
  inputs.precomputed_points_addr.tstrb <--. -1;
  for i = 0 to (num_tasks * scalar_num_bits) - 1 do
    let b0 = Char.to_int (Bytes.get scalar_stream ((2 * i) + 0)) in
    let b1 = Char.to_int (Bytes.get scalar_stream ((2 * i) + 1)) in
    let addr = (b1 lsl 8) lor b0 in
    inputs.precomputed_points_addr.tvalid <--. 1;
    inputs.precomputed_points_addr.tdata <--. addr;
    cycle_until ~timeout:1_000 sim (fun () ->
        Bits.is_vdd !(outputs_before.precomputed_points_addr_dest.tready));
    inputs.precomputed_points_addr.tvalid <--. 0;
    inputs.precomputed_points_addr.tdata <--. 0xFFFF;
    if wiggle_tvalid then cycle_for ~n:(Random.int 5) sim
  done;
  inputs.precomputed_points_addr.tvalid <--. 0;
  (* Wait until there is output data to read *)
  cycle_until ~timeout:1_000 sim (fun () -> Bits.is_vdd !(outputs.result_to_host.tvalid));
  (* Optionally, sleep for some number of cycles until accepting result. This
   * tests if the statemachine respects the result tready pushback.
   *)
  Option.iter sleep_for_cycles_until_accepting_result ~f:(fun n -> cycle_for ~n sim);
  (* Now, parse the output data. *)
  let rec loop () =
    let bits = Bytes.create 48 in
    for i = 0 to 2 do
      if wiggle_tvalid
      then (
        inputs.result_to_host_dest.tready <--. 0;
        cycle_for ~n:(Random.int 5) sim);
      assert (Bits.is_vdd !(outputs.result_to_host.tvalid));
      assert (Bits.width !(outputs.result_to_host.tdata) = 128);
      Bytes.blito
        ~dst:bits
        ~dst_pos:(i * 16)
        ~src:(Bits.Expert.unsafe_underlying_repr !(outputs.result_to_host.tdata))
        ~src_pos:Bits.Expert.offset_for_data
        ~src_len:16
        ();
      inputs.result_to_host_dest.tready <--. 1;
      Cyclesim.cycle sim
    done;
    inputs.result_to_host_dest.tready <--. 0;
    let bits =
      Z.of_bits (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:bits)
    in
    if Bits.is_vdd !(outputs_before.result_to_host.tlast)
    then [ bits ]
    else bits :: loop ()
  in
  let raw_result =
    List.chunks_of (loop ()) ~length:3
    |> List.map ~f:(function
           | [ x; y; z_and_is_infinity ] ->
             let x = transform_from_montgomery x in
             let y = transform_from_montgomery y in
             let z = transform_from_montgomery (Z.extract z_and_is_infinity 0 377) in
             let infinity =
               match Z.to_int (Z.extract z_and_is_infinity 377 1) with
               | 0 -> false
               | 1 -> true
               | _ -> assert false
             in
             let x = modulo_multiply x (modulo_inverse Z.(z ** 2)) in
             let y = modulo_multiply y (modulo_inverse Z.(z ** 3)) in
             Ark_bls12_377_g1.create ~x ~y ~infinity
           | _ -> assert false)
  in
  let obtained = List.reduce_exn ~f:Ark_bls12_377_g1.add raw_result in
  printf "Expected on curve?: %b\n" (Ark_bls12_377_g1.is_on_curve expected);
  printf "Obtained on curve?: %b\n" (Ark_bls12_377_g1.is_on_curve obtained);
  printf
    "Expected matches obtained?: %b\n"
    ([%equal: Ark_bls12_377_g1.affine] expected obtained);
  print_s
    [%message (expected : Ark_bls12_377_g1.affine) (obtained : Ark_bls12_377_g1.affine)];
  inputs.result_to_host_dest.tready <--. 0
;;

let%expect_test "Randomized standard test" =
  let waves, sim = Waveform.create (create_sim ()) in
  let dump_waveform () = Waveform.Serialize.marshall waves "a.hardcamlwaveform.Z" in
  let batch_size = 3 in
  let scalar_num_bits =
    (* scalar_num_bits = 9 is the smallest num_bits where things are more
     * interesting, as it validates if the setup_stream driver code works
     * appropriately across the byte boundary.
     *)
    9
  in
  let num_scalars = 20 in
  let scalars =
    List.concat
      [ List.init batch_size ~f:(Fn.const 0)
      ; List.init (num_scalars - batch_size) ~f:(fun _ ->
            Random.int_incl 1 ((1 lsl scalar_num_bits) - 1))
      ]
  in
  let field_points =
    let generator = Ark_bls12_377_g1.subgroup_generator () in
    List.init num_scalars ~f:(fun _ ->
        let by = Random.int_incl 1 ((1 lsl scalar_num_bits) - 1) in
        Ark_bls12_377_g1.mul ~by generator)
  in
  Core.protect
    ~f:(fun () ->
      test ~wiggle_tvalid:false ~batch_size ~scalar_num_bits ~scalars ~field_points sim)
    ~finally:dump_waveform;
  [%expect
    {|
    (scalars
     (0x0 0x0 0x0 0x157 0x4 0x176 0x184 0x1d0 0x48 0x16f 0x1ac 0x8e 0x23 0x1ba
      0xcd 0xdf 0x3f 0x31 0x162 0x19c))
    num scalars/field points: 20
    batch size: 3
    num tasks: 7
    Number of bytes in scalar stream: 126
    Expected on curve?: true
    Obtained on curve?: true
    Expected matches obtained?: true
    ((expected
      ((x
        0x17cde02fa5e87e787b6aba1b233de79016ae975c984c8e82216afb7b4a883f95483f64f9287fb1e70375fd3b0877e76)
       (y
        0x16f539eceeb5fef12ce0f0be154e0145f5051df9317ee5eb1eb06f74502129957b9a9af35f7b41d5de2057408629b05)
       (infinity false)))
     (obtained
      ((x
        0x17cde02fa5e87e787b6aba1b233de79016ae975c984c8e82216afb7b4a883f95483f64f9287fb1e70375fd3b0877e76)
       (y
        0x16f539eceeb5fef12ce0f0be154e0145f5051df9317ee5eb1eb06f74502129957b9a9af35f7b41d5de2057408629b05)
       (infinity false)))) |}]
;;
