open! Core
open Hardcaml
open Hardcaml_waveterm

module Model = struct
  open! Signal
  open Pippenger

  module Config = struct
    let window_size_bits = 4
    let num_windows = 2
    let affine_point_bits = 16
    let pipeline_depth = 7
    let log_num_scalars = 3
    let log_stall_fifo_depth = 2
  end

  include Config

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

  module Controller = Controller.Make (Config)

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
        scope
        { Controller.I.clock = i.clock
        ; clear = i.clear
        ; start = i.start
        ; scalar = i.scalar
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
                [| { read_clock = i.clock; read_address = ctrl.bucket; read_enable = vdd }
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
         ~pipeline_depth:(pipeline_depth - 1)
         { Pipe.I.clock = i.clock
         ; adder_a = mux2 ctrl.bubble (ones affine_point_bits) ctrl.adder_affine_point
         ; adder_b = mux ctrl.window bucket0
         ; bucket = ctrl.bucket
         ; window = ctrl.window
         ; valid = ctrl.execute &: ~:(ctrl.bubble)
         });
    { O.done_ = ctrl.done_
    ; bucket = mux (reg spec i.bucket_window) bucket1
    ; scalar_read = ctrl.scalar_read
    }
  ;;
end

module Sim = Cyclesim.With_interface (Model.I) (Model.O)

let ( <-. ) a b = a := Bits.of_int ~width:(Bits.width !a) b
let () = Random.set_state (Random.State.make [| 1 |])

module Msm_input = struct
  type 'a t =
    { scalar : 'a array [@length 0]
    ; affine_point : 'a
    }
  [@@deriving sexp_of, hardcaml]

  let random_inputs () =
    Array.init (1 lsl Model.Config.log_num_scalars) ~f:(fun _ ->
        { scalar =
            Array.init Model.Config.num_windows ~f:(fun _ ->
                Bits.random ~width:Model.Config.window_size_bits)
        ; affine_point = Bits.random ~width:Model.Config.affine_point_bits
        })
  ;;

  let of_scalars scalars =
    Array.map scalars ~f:(fun scalar ->
        { scalar =
            Array.init Model.Config.num_windows ~f:(fun w ->
                Bits.of_int
                  ~width:Model.Config.window_size_bits
                  (scalar lsr (w * Model.Config.window_size_bits)))
        ; affine_point = Bits.random ~width:Model.Config.affine_point_bits
        })
  ;;

  let sort_window_into_buckets (i : Bits.t t array) ~window =
    let a = Array.create ~len:(1 lsl Model.Config.window_size_bits) [] in
    Array.iter i ~f:(fun { scalar; affine_point } ->
        let index = Bits.to_int scalar.(window) in
        a.(index) <- affine_point :: a.(index));
    a
  ;;

  let sort_into_buckets (i : Bits.t t array) =
    Array.init Model.Config.num_windows ~f:(fun window ->
        sort_window_into_buckets i ~window)
  ;;

  let reduce0 ~f l =
    match l with
    | [] -> Bits.zero Model.Config.affine_point_bits
    | _ -> Bits.reduce ~f l
  ;;

  let sum_window (i : Bits.t list array) = Array.map i ~f:(reduce0 ~f:Bits.( +: ))
  let sum_all_windows = Array.map ~f:sum_window

  let print_results (i : Bits.t t array) =
    let buckets = sort_into_buckets i in
    let sums = sum_all_windows buckets in
    let buckets = Array.map buckets ~f:(Array.map ~f:(List.map ~f:Bits.to_int)) in
    let sums = Array.map sums ~f:(Array.map ~f:Bits.to_int) in
    print_s
      [%message (buckets : Int.Hex.t list array array) (sums : Int.Hex.t array array)]
  ;;
end

let poll ~timeout ~f cycle =
  let t = ref 0 in
  while (not (f ())) && !t < timeout do
    cycle ();
    Int.incr t
  done
;;

let test ?(auto_label_hierarchical_ports = true) coefs =
  print_s
    [%message
      (Array.map coefs ~f:(Msm_input.map ~f:Bits.to_int) : Int.Hex.t Msm_input.t array)];
  let sim =
    Sim.create
      ~config:Cyclesim.Config.trace_all
      (Model.create (Scope.create ~flatten_design:true ~auto_label_hierarchical_ports ()))
  in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle_num = ref 0 in
  let results =
    Array.init Model.Config.num_windows ~f:(fun _ ->
        Array.init (1 lsl Model.Config.window_size_bits) ~f:(Fn.const 0))
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
  inputs.clear <-. 1;
  cycle ();
  inputs.clear <-. 0;
  inputs.start <-. 1;
  cycle ();
  inputs.start <-. 0;
  inputs.scalar_valid <-. 1;
  let num_coefs = Array.length coefs in
  for i = 0 to num_coefs - 1 do
    (* print_s [%message (i : int) (!cycle_num : int)]; *)
    if i = num_coefs - 1 then inputs.last_scalar := Bits.vdd;
    for w = 0 to Model.num_windows - 1 do
      inputs.scalar.(w) := coefs.(i).scalar.(w)
    done;
    inputs.affine_point := coefs.(i).affine_point;
    poll ~timeout:1_000 ~f:(fun () -> Bits.to_bool !(outputs.scalar_read)) cycle;
    cycle ()
  done;
  inputs.scalar_valid <-. 0;
  poll ~timeout:1_000 ~f:(fun () -> Bits.to_bool !(outputs.done_)) cycle;
  (* run to flush pipeline, plus a few cycles *)
  for _ = 0 to Model.Config.pipeline_depth + 10 do
    cycle ()
  done;
  (* Read back windows *)
  for window = 0 to Model.Config.num_windows - 1 do
    for bucket = 0 to (1 lsl Model.Config.window_size_bits) - 1 do
      inputs.bucket_address <-. bucket;
      inputs.bucket_window <-. window;
      inputs.bucket_read_enable <-. 1;
      cycle ()
    done
  done;
  inputs.bucket_read_enable <-. 0;
  cycle ();
  print_s [%message (results : Int.Hex.t array array)];
  let final_sum =
    Array.foldi results ~init:0 ~f:(fun index acc window ->
        let r = Array.foldi window ~init:0 ~f:(fun index acc e -> acc + (e * index)) in
        acc + (r lsl (index * Model.Config.window_size_bits)))
    land ((1 lsl Model.Config.affine_point_bits) - 1)
  in
  let expected_sum =
    Array.fold
      coefs
      ~init:(Bits.zero Model.Config.(affine_point_bits))
      ~f:(fun acc { scalar; affine_point } ->
        Bits.(
          acc
          +: sel_bottom
               ((Array.to_list scalar |> concat_lsb) *: affine_point)
               Model.Config.affine_point_bits))
    |> Bits.to_int
  in
  print_s [%message (final_sum : Int.Hex.t) (expected_sum : Int.Hex.t)];
  waves
;;

let test_with_stalls =
  Msm_input.of_scalars [| 0x12; 0x21; 0x32; 0xb4; 0x16; 0xac; 0xff; 0x41 |]
;;

let test_no_stalls =
  Msm_input.of_scalars [| 0x12; 0x34; 0x56; 0x78; 0x9a; 0xbc; 0xde; 0xf0 |]
;;

let%expect_test "example" =
  let waves = test ~auto_label_hierarchical_ports:false test_with_stalls in
  Msm_input.print_results test_with_stalls;
  Waveform.print ~display_height:50 ~display_width:135 ~wave_width:0 waves;
  [%expect
    {|
    ("Array.map coefs ~f:(Msm_input.map ~f:Bits.to_int)"
     (((scalar (0x2 0x1)) (affine_point 0xef36))
      ((scalar (0x1 0x2)) (affine_point 0xac3e))
      ((scalar (0x2 0x3)) (affine_point 0xe2e0))
      ((scalar (0x4 0xb)) (affine_point 0x7772))
      ((scalar (0x6 0x1)) (affine_point 0x5fc3))
      ((scalar (0xc 0xa)) (affine_point 0xd5ac))
      ((scalar (0xf 0xf)) (affine_point 0x7ab2))
      ((scalar (0x1 0x4)) (affine_point 0x53de))))
    (results
     ((0x0 0x1c 0xe2e0 0x0 0x7772 0x0 0x5fc3 0x0 0x0 0x0 0x0 0x0 0xd5ac 0x0 0x0
       0x7ab2)
      (0x0 0x5fc3 0xac3e 0xe2e0 0x53de 0x0 0x0 0x0 0x0 0x0 0xd5ac 0x7772 0x0 0x0
       0x0 0x7ab2)))
    ((final_sum 0xcce4) (expected_sum 0x9eb0))
    ((buckets
      ((() (0x53de 0xac3e) (0xe2e0 0xef36) () (0x7772) () (0x5fc3) () () () () ()
        (0xd5ac) () () (0x7ab2))
       (() (0x5fc3 0xef36) (0xac3e) (0xe2e0) (0x53de) () () () () () (0xd5ac)
        (0x7772) () () () (0x7ab2))))
     (sums
      ((0x0 0x1c 0xd216 0x0 0x7772 0x0 0x5fc3 0x0 0x0 0x0 0x0 0x0 0xd5ac 0x0 0x0
        0x7ab2)
       (0x0 0x4ef9 0xac3e 0xe2e0 0x53de 0x0 0x0 0x0 0x0 0x0 0xd5ac 0x7772 0x0 0x0
        0x0 0x7ab2))))
    ┌Signals───────────┐┌Waves────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
    │                  ││────┬───────┬───────┬───────┬───────┬───────┬───────────────────────────────┬───────┬────────────────────────────│
    │i$affine_point    ││ 00.│EF36   │AC3E   │E2E0   │7772   │5FC3   │D5AC                           │7AB2   │53DE                        │
    │                  ││────┴───────┴───────┴───────┴───────┴───────┴───────────────────────────────┴───────┴────────────────────────────│
    │                  ││─────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │i$bucket_address  ││ 0                                                                                                               │
    │                  ││─────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │i$bucket_read_enab││                                                                                                                 │
    │                  ││─────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │i$bucket_window   ││                                                                                                                 │
    │                  ││─────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │i$clear           ││──┐                                                                                                              │
    │                  ││  └──────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │i$clock           ││                                                                                                                 │
    │                  ││─────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │i$last_scalar     ││                                                                                    ┌────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────────────────┘                            │
    │                  ││────┬───────┬───────┬───────┬───────┬───────┬───────────────────────────────┬───────┬────────────────────────────│
    │i$scalar0         ││ 0  │2      │1      │2      │4      │6      │C                              │F      │1                           │
    │                  ││────┴───────┴───────┴───────┴───────┴───────┴───────────────────────────────┴───────┴────────────────────────────│
    │                  ││────┬───────┬───────┬───────┬───────┬───────┬───────────────────────────────┬───────┬────────────────────────────│
    │i$scalar1         ││ 0  │1      │2      │3      │B      │1      │A                              │F      │4                           │
    │                  ││────┴───────┴───────┴───────┴───────┴───────┴───────────────────────────────┴───────┴────────────────────────────│
    │i$scalar_valid    ││    ┌───────────────────────────────────────────────────────────────────────────────────────┐                    │
    │                  ││────┘                                                                                       └────────────────────│
    │i$start           ││  ┌─┐                                                                                                            │
    │                  ││──┘ └────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││─────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │o$bucket          ││ 0000                                                                                                            │
    │                  ││─────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │o$done_           ││────┐                                                                                                 ┌──────────│
    │                  ││    └─────────────────────────────────────────────────────────────────────────────────────────────────┘          │
    │o$scalar_read     ││          ┌─┐     ┌─┐     ┌─┐     ┌─┐     ┌─┐                             ┌─┐     ┌─┐     ┌─┐                    │
    │                  ││──────────┘ └─────┘ └─────┘ └─────┘ └─────┘ └─────────────────────────────┘ └─────┘ └─────┘ └────────────────────│
    │                  ││────┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬──────────│
    │ctrl$STATE        ││ 0  │1│3│4│3│1│3│4│3│1│3│4│3│1│3│4│3│1│3│4│3│1│5│6│5│1│5│6│5│1│5│6│5│1│3│4│3│1│3│4│3│1│3│4│3│1│5│6│5│1│0         │
    │                  ││────┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴──────────│
    │ctrl$exec_scalar  ││      ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐                         ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐                    │
    │                  ││──────┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─────────────────────────┘ └─┘ └─┘ └─┘ └─┘ └─┘ └────────────────────│
    │ctrl$exec_stalled ││                                              ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐                         ┌─┐ ┌─┐            │
    │                  ││──────────────────────────────────────────────┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─────────────────────────┘ └─┘ └────────────│
    │ctrl$is_in_pipelin││────┐               ┌───┐               ┌───┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐     ┌─┐                       ┌─────┐ ┌────────────│
    │                  ││    └───────────────┘   └───────────────┘   └─┘ └─┘ └─┘ └─┘ └─────┘ └───────────────────────┘     └─┘            │
    │ctrl$pop          ││                                                              ┌─┐                                 ┌─┐            │
    │                  ││──────────────────────────────────────────────────────────────┘ └─────────────────────────────────┘ └────────────│
    │ctrl$push         ││                      ┌─┐                 ┌─┐                                                                    │
    │                  ││──────────────────────┘ └─────────────────┘ └────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────┬───────────────────────────────────────┬────────────────────────────────────────────────│
    │ctrl$stalled_point││ 0                      │1                                      │0                                               │
    └──────────────────┘└─────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘ |}]
;;
