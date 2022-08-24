open! Core
open Hardcaml
open Hardcaml_waveterm

module Model = struct
  open! Signal
  open Pippenger

  module Config = struct
    let window_size_bits = 8
    let num_windows = 4
    let affine_point_bits = 16
    let pipeline_depth = 20
    let log_num_scalars = 5
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
        ; addr_a : 'a [@bits affine_point_bits]
        ; addr_b : 'a [@bits affine_point_bits]
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
      { O.result = pipeline (i.addr_a +: i.addr_b)
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
                 ; read_address = i.scalar.(window)
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
         ~pipeline_depth:(pipeline_depth - 1)
         { Pipe.I.clock = i.clock
         ; addr_a =
             mux2 (reg spec ctrl.bubble) (ones affine_point_bits) ctrl.adder_affine_point
         ; addr_b = mux ctrl.window bucket0
         ; bucket = reg spec ctrl.bucket
         ; window = reg spec ctrl.window
         ; valid = reg spec ctrl.execute
         });
    { O.done_ = ctrl.done_
    ; bucket = mux (reg spec i.bucket_window) bucket1
    ; scalar_read = ctrl.scalar_read
    }
  ;;
end

module Sim = Cyclesim.With_interface (Model.I) (Model.O)

let ( <-. ) a b = a := Bits.of_int ~width:(Bits.width !a) b

let test ?(auto_label_hierarchical_ports = true) () =
  let sim =
    Sim.create
      ~config:Cyclesim.Config.trace_all
      (Model.create (Scope.create ~flatten_design:true ~auto_label_hierarchical_ports ()))
  in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  inputs.clear <-. 1;
  Cyclesim.cycle sim;
  inputs.clear <-. 0;
  inputs.start <-. 1;
  Cyclesim.cycle sim;
  inputs.start <-. 0;
  inputs.scalar_valid <-. 1;
  for i = 0 to (1 lsl Model.log_num_scalars) - 1 do
    for w = 0 to Model.num_windows - 1 do
      inputs.scalar.(w) <-. (i + 1 + w) % 3 * (w + 1)
    done;
    inputs.affine_point <-. i + 0x10;
    for _ = 0 to Model.num_windows - 1 do
      Cyclesim.cycle sim
    done
  done;
  inputs.scalar_valid <-. 0;
  waves
;;

let%expect_test "" =
  let waves = test ~auto_label_hierarchical_ports:false () in
  Waveform.print ~display_height:50 ~display_width:120 ~wave_width:2 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────────────────────────┐
    │                  ││────────────┬───────────────────────┬───────────────────────┬───────────────────────┬─────────────│
    │i$affine_point    ││ 0000       │0010                   │0011                   │0012                   │0013         │
    │                  ││────────────┴───────────────────────┴───────────────────────┴───────────────────────┴─────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │i$bucket_address  ││ 00                                                                                               │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │i$bucket_read_enab││                                                                                                  │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │i$bucket_window   ││ 0                                                                                                │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │i$clear           ││──────┐                                                                                           │
    │                  ││      └───────────────────────────────────────────────────────────────────────────────────────────│
    │i$clock           ││                                                                                                  │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────┬───────────────────────┬───────────────────────┬───────────────────────┬─────────────│
    │i$scalar0         ││ 00         │01                     │02                     │00                     │01           │
    │                  ││────────────┴───────────────────────┴───────────────────────┴───────────────────────┴─────────────│
    │                  ││────────────┬───────────────────────┬───────────────────────┬───────────────────────┬─────────────│
    │i$scalar1         ││ 00         │04                     │00                     │02                     │04           │
    │                  ││────────────┴───────────────────────┴───────────────────────┴───────────────────────┴─────────────│
    │                  ││────────────────────────────────────┬───────────────────────┬───────────────────────┬─────────────│
    │i$scalar2         ││ 00                                 │03                     │06                     │00           │
    │                  ││────────────────────────────────────┴───────────────────────┴───────────────────────┴─────────────│
    │                  ││────────────┬───────────────────────┬───────────────────────┬───────────────────────┬─────────────│
    │i$scalar3         ││ 00         │04                     │08                     │00                     │04           │
    │                  ││────────────┴───────────────────────┴───────────────────────┴───────────────────────┴─────────────│
    │i$scalar_valid    ││            ┌─────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────┘                                                                                     │
    │i$start           ││      ┌─────┐                                                                                     │
    │                  ││──────┘     └─────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │o$bucket          ││ 0000                                                                                             │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │o$done_           ││────────────┐                                                                                     │
    │                  ││            └─────────────────────────────────────────────────────────────────────────────────────│
    │o$scalar_read     ││                              ┌─────┐                 ┌─────┐                 ┌─────┐             │
    │                  ││──────────────────────────────┘     └─────────────────┘     └─────────────────┘     └─────────────│
    │                  ││────────────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─│
    │ctrl$PIPE_SHIFT   ││ 0          │1    │2    │4    │8    │1    │2    │4    │8    │1    │2    │4    │8    │1    │2    │4│
    │                  ││────────────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─│
    │                  ││──────────────────┬───────────────────────┬───────────────────────┬───────────────────────┬───────│
    │ctrl$SCLR_CNT     ││ 00               │01                     │02                     │03                     │04     │
    │                  ││──────────────────┴───────────────────────┴───────────────────────┴───────────────────────┴───────│
    │                  ││────────────┬─────┬─────────────────┬─────┬─────────────────┬─────┬─────────────────┬─────┬───────│
    │ctrl$STATE        ││ 0          │1    │2                │1    │2                │1    │2                │1    │2      │
    │                  ││────────────┴─────┴─────────────────┴─────┴─────────────────┴─────┴─────────────────┴─────┴───────│
    │ctrl$is_in_pipelin││────────────┐           ┌─────┐           ┌─────┐           ┌─────┐           ┌───────────────────│
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────────────────────────┘ |}]
;;
