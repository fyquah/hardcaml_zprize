open! Core
open Hardcaml
open Hardcaml_waveterm
module Scalar = Pippenger.Scalar
module Scalar_config = Scalar.Scalar_config

module type Config = sig
  include Pippenger.Config.S

  val datapath_depth : int
end

module Model (Config : Config) (Scalar_config : Scalar_config.S) = struct
  include Config
  include Scalar_config
  open! Signal

  let () = Caller_id.set_mode Full_trace
  let log_num_windows = Int.ceil_log2 num_windows

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; scalar : 'a array [@bits window_size_bits] [@length num_windows]
      ; scalar_valid : 'a
      ; last_scalar : 'a
      ; affine_point : 'a [@bits affine_point_bits]
      ; bucket_read_enable : 'a
      ; bucket_address : 'a [@bits window_size_bits]
      ; bucket_window : 'a [@bits log_num_windows]
      }
    [@@deriving sexp_of, hardcaml ~rtlprefix:"i$"]
  end

  module O = struct
    type 'a t =
      { done_ : 'a
      ; bucket : 'a [@bits affine_point_bits]
      ; scalar_read : 'a
      }
    [@@deriving sexp_of, hardcaml ~rtlprefix:"o$"]
  end

  module Controller = Pippenger.Controller.Make (Config) (Scalar_config)
  module Scalar = Controller.Scalar

  module Pipe = struct
    module I = struct
      type 'a t =
        { clock : 'a
        ; adder_a : 'a [@bits affine_point_bits]
        ; adder_b : 'a [@bits affine_point_bits]
        ; bucket : 'a [@bits window_size_bits]
        ; window : 'a [@bits log_num_windows]
        ; valid : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t =
        { result : 'a [@bits affine_point_bits]
        ; bucket : 'a [@bits window_size_bits] [@rtlsuffix "_o"]
        ; window : 'a [@bits log_num_windows] [@rtlsuffix "_o"]
        ; write_enable : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    let create _scope ~pipeline_depth (i : _ I.t) =
      let spec = Reg_spec.create ~clock:i.clock () in
      let pipeline = pipeline spec ~n:pipeline_depth in
      { O.result = pipeline (i.adder_a +: i.adder_b)
      ; bucket = pipeline i.bucket
      ; window = pipeline i.window
      ; write_enable = pipeline i.valid
      }
    ;;

    let hierarchy scope ~pipeline_depth =
      let module Hier = Hierarchy.In_scope (I) (O) in
      Hier.hierarchical ~name:"dp" ~scope (create ~pipeline_depth)
    ;;
  end

  let create scope (i : _ I.t) =
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let dp = Pipe.O.Of_signal.wires () in
    let ctrl =
      Controller.hierarchy
        ~build_mode:Simulation
        scope
        { Controller.I.clock = i.clock
        ; clear = i.clear
        ; start = i.start
        ; scalar = Array.map i.scalar ~f:(fun scalar -> { Scalar.scalar; negative = gnd })
        ; scalar_valid = i.scalar_valid
        ; last_scalar = i.last_scalar
        ; affine_point = i.affine_point
        }
    in
    let bucket0, bucket1 =
      List.init num_windows ~f:(fun window ->
        let q =
          Ram.create
            ~collision_mode:Write_before_read
            ~size:(1 lsl window_size_bits)
            ~read_ports:
              [| { read_clock = i.clock
                 ; read_address = ctrl.bucket.scalar
                 ; read_enable = vdd
                 }
               ; { read_clock = i.clock
                 ; read_address = i.bucket_address
                 ; read_enable = i.bucket_read_enable
                 }
              |]
            ~write_ports:
              [| { write_clock = i.clock
                 ; write_address = dp.bucket
                 ; write_enable = dp.write_enable &: (dp.window ==:. window)
                 ; write_data = dp.result
                 }
              |]
            ()
        in
        q.(0), q.(1))
      |> List.unzip
    in
    (* This is a basic model of the affine point adder. *)
    Pipe.O.Of_signal.assign
      dp
      (Pipe.hierarchy
         scope
         ~pipeline_depth:datapath_depth
         { Pipe.I.clock = i.clock
         ; adder_a = mux2 ctrl.bubble (ones affine_point_bits) ctrl.adder_affine_point
         ; adder_b = mux ctrl.window bucket0
         ; bucket = ctrl.bucket.scalar
         ; window = ctrl.window
         ; valid = ctrl.execute &: ~:(ctrl.bubble)
         });
    { O.done_ = ctrl.done_
    ; bucket = mux (reg spec i.bucket_window) bucket1
    ; scalar_read = ctrl.scalar_read
    }
  ;;
end

module Msm_input = struct
  type 'a t =
    { scalar : 'a array [@length 0]
    ; affine_point : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module Test (Config : Config) (Scalar_config : Scalar_config.S) = struct
  module Model = Model (Config) (Scalar_config)
  module Sim = Cyclesim.With_interface (Model.I) (Model.O)

  let ( <-. ) a b = a := Bits.of_int ~width:(Bits.width !a) b

  let random_inputs num_scalars =
    Array.init num_scalars ~f:(fun _ ->
      { Msm_input.scalar =
          Array.init Model.num_windows ~f:(fun _ ->
            Bits.random ~width:Model.window_size_bits)
      ; affine_point = Bits.random ~width:Model.affine_point_bits
      })
  ;;

  let debug_inputs num_scalars =
    Array.init num_scalars ~f:(fun idx ->
      { Msm_input.scalar =
          Array.init Model.num_windows ~f:(fun _ ->
            Bits.random ~width:Model.window_size_bits)
      ; affine_point = Bits.of_int ~width:Model.affine_point_bits (idx + 1)
      })
  ;;

  let of_scalars scalars =
    Array.mapi scalars ~f:(fun idx scalar ->
      { Msm_input.scalar =
          Array.init Model.num_windows ~f:(fun w ->
            Bits.of_int
              ~width:Model.window_size_bits
              (scalar lsr (w * Model.window_size_bits)))
      ; affine_point = Bits.of_int ~width:Model.affine_point_bits (idx + 1)
      })
  ;;

  let sort_window_into_buckets (i : Bits.t Msm_input.t array) ~window =
    let a = Array.create ~len:(1 lsl Model.window_size_bits) [] in
    Array.iter i ~f:(fun { scalar; affine_point } ->
      let index = Bits.to_int scalar.(window) in
      a.(index) <- affine_point :: a.(index));
    a
  ;;

  let sort_into_buckets (i : Bits.t Msm_input.t array) =
    Array.init Model.num_windows ~f:(fun window -> sort_window_into_buckets i ~window)
  ;;

  let reduce0 ~f l =
    match l with
    | [] -> Bits.zero Model.affine_point_bits
    | _ -> Bits.reduce ~f l
  ;;

  let sum_window (i : Bits.t list array) = Array.map i ~f:(reduce0 ~f:Bits.( +: ))
  let sum_all_windows = Array.map ~f:sum_window

  let compare_hw_and_sw_buckets hw sw =
    for window = 0 to Array.length sw - 1 do
      for bucket = 0 to Array.length sw.(0) - 1 do
        let hw = hw.(window).(bucket) in
        let sw = sw.(window).(bucket) in
        if hw <> sw
        then
          print_s
            [%message
              "mismatched bucket"
                (window : int)
                (bucket : int)
                (hw : Int.Hex.t)
                (sw : Int.Hex.t)]
      done
    done
  ;;

  let print_results (i : Bits.t Msm_input.t array) hw_results =
    let buckets = sort_into_buckets i in
    let sums = sum_all_windows buckets in
    let buckets = Array.map buckets ~f:(Array.map ~f:(List.map ~f:Bits.to_int)) in
    let sums = Array.map sums ~f:(Array.map ~f:Bits.to_int) in
    Array.iter sums ~f:(fun s -> s.(0) <- 0);
    print_s
      [%message
        "REFERENCE" (sums : Int.Hex.t array array) (buckets : Int.Hex.t list array array)];
    compare_hw_and_sw_buckets hw_results sums
  ;;

  let poll ~timeout ~f cycle =
    let t = ref 0 in
    while (not (f ())) && !t < timeout do
      cycle ();
      Int.incr t
    done
  ;;

  let test
    ?(waves = false)
    ?(verbose = false)
    ?(auto_label_hierarchical_ports = true)
    ?(can_stall = false)
    coefs
    =
    if verbose
    then (
      let inputs = Array.map coefs ~f:(Msm_input.map ~f:Bits.to_int) in
      print_s [%message (inputs : Int.Hex.t Msm_input.t array)]);
    let sim =
      Sim.create
        ~config:{ Cyclesim.Config.trace_all with deduplicate_signals = false }
        (Model.create
           (Scope.create ~flatten_design:true ~auto_label_hierarchical_ports ()))
    in
    let waveform, sim =
      if waves
      then (
        let waves, sim = Waveform.create sim in
        Some waves, sim)
      else None, sim
    in
    let inputs = Cyclesim.inputs sim in
    let outputs = Cyclesim.outputs sim in
    let cycle_num = ref 0 in
    let results =
      Array.init Model.num_windows ~f:(fun _ ->
        Array.init (1 lsl Model.window_size_bits) ~f:(Fn.const 0))
    in
    let cycle =
      let b = ref 0 in
      let w = ref 0 in
      fun () ->
        let ren = Bits.to_bool !(inputs.bucket_read_enable) in
        if ren
        then (
          b := Bits.to_int !(inputs.bucket_address);
          w := Bits.to_int !(inputs.bucket_window));
        Int.incr cycle_num;
        Cyclesim.cycle sim;
        if ren
        then (
          let r = Bits.to_int !(outputs.bucket) in
          results.(!w).(!b) <- r)
    in
    let cycle ?(n = 1) () =
      for _ = 1 to n do
        cycle ()
      done
    in
    inputs.clear <-. 1;
    cycle ();
    inputs.clear <-. 0;
    inputs.start <-. 1;
    cycle ();
    inputs.start <-. 0;
    let num_coefs = Array.length coefs in
    for i = 0 to num_coefs - 1 do
      if can_stall then cycle ~n:(1 + Random.int 5) ();
      inputs.scalar_valid <-. 1;
      (* print_s [%message (i : int) (!cycle_num : int)]; *)
      if i = num_coefs - 1 then inputs.last_scalar := Bits.vdd;
      for w = 0 to Model.num_windows - 1 do
        inputs.scalar.(w) := coefs.(i).scalar.(w)
      done;
      inputs.affine_point := coefs.(i).affine_point;
      poll ~timeout:1_000 ~f:(fun () -> Bits.to_bool !(outputs.scalar_read)) cycle;
      cycle ();
      inputs.scalar_valid <-. 0
    done;
    inputs.scalar_valid <-. 0;
    poll ~timeout:1_000 ~f:(fun () -> Bits.to_bool !(outputs.done_)) cycle;
    (* run to flush pipeline, plus a few cycles *)
    for _ = 0 to (Model.datapath_depth * 2) + 10 do
      cycle ()
    done;
    (* Read back windows *)
    for window = 0 to Model.num_windows - 1 do
      for bucket = 0 to (1 lsl Model.window_size_bits) - 1 do
        inputs.bucket_address <-. bucket;
        inputs.bucket_window <-. window;
        inputs.bucket_read_enable <-. 1;
        cycle ()
      done
    done;
    inputs.bucket_read_enable <-. 0;
    cycle ();
    if verbose then print_s [%message "HW-RESULTS" ~_:(results : Int.Hex.t array array)];
    let final_sum =
      Array.foldi results ~init:0 ~f:(fun index acc window ->
        let r = Array.foldi window ~init:0 ~f:(fun index acc e -> acc + (e * index)) in
        acc + (r lsl (index * Model.window_size_bits)))
      land ((1 lsl Model.affine_point_bits) - 1)
    in
    let expected_sum =
      Array.fold
        coefs
        ~init:(Bits.zero Model.(affine_point_bits))
        ~f:(fun acc { scalar; affine_point } ->
          Bits.(
            acc
            +: sel_bottom
                 ((Array.to_list scalar |> concat_lsb) *: affine_point)
                 Model.affine_point_bits))
      |> Bits.to_int
    in
    if verbose
    then (
      print_results coefs results;
      print_s
        [%message
          "RESULTS" (final_sum : Int.Hex.t) (expected_sum : Int.Hex.t) (!cycle_num : int)]);
    if final_sum <> expected_sum
    then (
      let m = [%message "TEST FAILED :("] in
      if waves then print_s m else raise_s m);
    waveform
  ;;
end

module Config = struct
  let num_windows = 2
  let affine_point_bits = 16
  let datapath_depth = 8
  let pipeline_depth = (datapath_depth + 1) / 2
  let log_stall_fifo_depth = 2
end

module Scalar_config_ = struct
  let window_size_bits = 4
end

module Simple_model = Test (Config) (Scalar_config_)

let test_with_stalls =
  Simple_model.of_scalars [| 0x12; 0x21; 0x32; 0xb4; 0x16; 0xac; 0xff; 0x41 |]
;;

let test_no_stalls =
  Simple_model.of_scalars [| 0x12; 0x34; 0x56; 0x78; 0x9a; 0xbc; 0xde; 0xf0 |]
;;

let test_1_stall =
  Simple_model.of_scalars [| 0x21; 0x43; 0x61; 0x87; 0xa9; 0xcb; 0xed; 0x0f |]
;;

let test_fully_stall_window0 =
  Simple_model.of_scalars [| 0x13; 0x23; 0x33; 0x43; 0x53; 0x63; 0x73; 0x83 |]
;;

let test_with_twenty_ones = Simple_model.of_scalars [| 0x21; 0x21 |]
let test = Simple_model.test

let runtest ?(can_stall = false) ?(waves = false) example =
  let waves =
    Simple_model.test
      ~waves
      ~verbose:true
      ~auto_label_hierarchical_ports:false
      ~can_stall
      example
  in
  Option.iter
    waves
    ~f:(Waveform.print ~display_height:50 ~display_width:135 ~wave_width:0)
;;

let%expect_test "no stalls" =
  runtest test_no_stalls;
  [%expect
    {|
    (inputs
     (((scalar (0x2 0x1)) (affine_point 0x1))
      ((scalar (0x4 0x3)) (affine_point 0x2))
      ((scalar (0x6 0x5)) (affine_point 0x3))
      ((scalar (0x8 0x7)) (affine_point 0x4))
      ((scalar (0xa 0x9)) (affine_point 0x5))
      ((scalar (0xc 0xb)) (affine_point 0x6))
      ((scalar (0xe 0xd)) (affine_point 0x7))
      ((scalar (0x0 0xf)) (affine_point 0x8))))
    (HW-RESULTS
     ((0x0 0x0 0x1 0x0 0x2 0x0 0x3 0x0 0x4 0x0 0x5 0x0 0x6 0x0 0x7 0x0)
      (0x0 0x1 0x0 0x2 0x0 0x3 0x0 0x4 0x0 0x5 0x0 0x6 0x0 0x7 0x0 0x8)))
    (REFERENCE
     (sums
      ((0x0 0x0 0x1 0x0 0x2 0x0 0x3 0x0 0x4 0x0 0x5 0x0 0x6 0x0 0x7 0x0)
       (0x0 0x1 0x0 0x2 0x0 0x3 0x0 0x4 0x0 0x5 0x0 0x6 0x0 0x7 0x0 0x8)))
     (buckets
      (((0x8) () (0x1) () (0x2) () (0x3) () (0x4) () (0x5) () (0x6) () (0x7) ())
       (() (0x1) () (0x2) () (0x3) () (0x4) () (0x5) () (0x6) () (0x7) () (0x8)))))
    (RESULTS (final_sum 0x1858) (expected_sum 0x1858) (!cycle_num 95)) |}]
;;

let%expect_test "1 stalls" =
  runtest test_1_stall;
  [%expect
    {|
    (inputs
     (((scalar (0x1 0x2)) (affine_point 0x1))
      ((scalar (0x3 0x4)) (affine_point 0x2))
      ((scalar (0x1 0x6)) (affine_point 0x3))
      ((scalar (0x7 0x8)) (affine_point 0x4))
      ((scalar (0x9 0xa)) (affine_point 0x5))
      ((scalar (0xb 0xc)) (affine_point 0x6))
      ((scalar (0xd 0xe)) (affine_point 0x7))
      ((scalar (0xf 0x0)) (affine_point 0x8))))
    (HW-RESULTS
     ((0x0 0x4 0x0 0x2 0x0 0x0 0x0 0x4 0x0 0x5 0x0 0x6 0x0 0x7 0x0 0x8)
      (0x0 0x0 0x1 0x0 0x2 0x0 0x3 0x0 0x4 0x0 0x5 0x0 0x6 0x0 0x7 0x0)))
    (REFERENCE
     (sums
      ((0x0 0x4 0x0 0x2 0x0 0x0 0x0 0x4 0x0 0x5 0x0 0x6 0x0 0x7 0x0 0x8)
       (0x0 0x0 0x1 0x0 0x2 0x0 0x3 0x0 0x4 0x0 0x5 0x0 0x6 0x0 0x7 0x0)))
     (buckets
      ((() (0x3 0x1) () (0x2) () () () (0x4) () (0x5) () (0x6) () (0x7) () (0x8))
       ((0x8) () (0x1) () (0x2) () (0x3) () (0x4) () (0x5) () (0x6) () (0x7) ()))))
    (RESULTS (final_sum 0x12e8) (expected_sum 0x12e8) (!cycle_num 99)) |}]
;;

let%expect_test "has stalls" =
  runtest test_with_stalls;
  [%expect
    {|
    (inputs
     (((scalar (0x2 0x1)) (affine_point 0x1))
      ((scalar (0x1 0x2)) (affine_point 0x2))
      ((scalar (0x2 0x3)) (affine_point 0x3))
      ((scalar (0x4 0xb)) (affine_point 0x4))
      ((scalar (0x6 0x1)) (affine_point 0x5))
      ((scalar (0xc 0xa)) (affine_point 0x6))
      ((scalar (0xf 0xf)) (affine_point 0x7))
      ((scalar (0x1 0x4)) (affine_point 0x8))))
    (HW-RESULTS
     ((0x0 0xa 0x4 0x0 0x4 0x0 0x5 0x0 0x0 0x0 0x0 0x0 0x6 0x0 0x0 0x7)
      (0x0 0x6 0x2 0x3 0x8 0x0 0x0 0x0 0x0 0x0 0x6 0x4 0x0 0x0 0x0 0x7)))
    (REFERENCE
     (sums
      ((0x0 0xa 0x4 0x0 0x4 0x0 0x5 0x0 0x0 0x0 0x0 0x0 0x6 0x0 0x0 0x7)
       (0x0 0x6 0x2 0x3 0x8 0x0 0x0 0x0 0x0 0x0 0x6 0x4 0x0 0x0 0x0 0x7)))
     (buckets
      ((() (0x8 0x2) (0x3 0x1) () (0x4) () (0x5) () () () () () (0x6) () ()
        (0x7))
       (() (0x5 0x1) (0x2) (0x3) (0x8) () () () () () (0x6) (0x4) () () () (0x7)))))
    (RESULTS (final_sum 0x1131) (expected_sum 0x1131) (!cycle_num 99)) |}]
;;

let%expect_test "fully stall window 0" =
  runtest ~can_stall:false test_fully_stall_window0;
  [%expect
    {|
    (inputs
     (((scalar (0x3 0x1)) (affine_point 0x1))
      ((scalar (0x3 0x2)) (affine_point 0x2))
      ((scalar (0x3 0x3)) (affine_point 0x3))
      ((scalar (0x3 0x4)) (affine_point 0x4))
      ((scalar (0x3 0x5)) (affine_point 0x5))
      ((scalar (0x3 0x6)) (affine_point 0x6))
      ((scalar (0x3 0x7)) (affine_point 0x7))
      ((scalar (0x3 0x8)) (affine_point 0x8))))
    (HW-RESULTS
     ((0x0 0x0 0x0 0x24 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0)
      (0x0 0x1 0x2 0x3 0x4 0x5 0x6 0x7 0x8 0x0 0x0 0x0 0x0 0x0 0x0 0x0)))
    (REFERENCE
     (sums
      ((0x0 0x0 0x0 0x24 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0)
       (0x0 0x1 0x2 0x3 0x4 0x5 0x6 0x7 0x8 0x0 0x0 0x0 0x0 0x0 0x0 0x0)))
     (buckets
      ((() () () (0x8 0x7 0x6 0x5 0x4 0x3 0x2 0x1) () () () () () () () () () ()
        () ())
       (() (0x1) (0x2) (0x3) (0x4) (0x5) (0x6) (0x7) (0x8) () () () () () () ()))))
    (RESULTS (final_sum 0xd2c) (expected_sum 0xd2c) (!cycle_num 151)) |}]
;;
