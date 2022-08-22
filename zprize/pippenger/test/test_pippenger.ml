open! Core
open Hardcaml
open Hardcaml_waveterm

module Model = struct
  open! Signal
  open Pippenger
  include Config.Zprize

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
      }
    [@@deriving sexp_of, hardcaml ~rtlprefix:"o$"]
  end

  module Controller = Controller.Make (Config.Zprize)

  let create scope (i : _ I.t) =
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let ctrl =
      Controller.create
        scope
        { Controller.I.clock = i.clock
        ; clear = i.clear
        ; start = i.start
        ; scalar = i.scalar
        ; scalar_valid = i.scalar_valid
        ; affine_point = i.affine_point
        }
    in
    let adder = wire affine_point_bits in
    let bucket0, bucket1 =
      List.init num_windows ~f:(fun _ ->
          let q =
            Ram.create
              ~collision_mode:Write_before_read
              ~size:(1 lsl window_size_bits)
              ~read_ports:
                [| { read_clock = i.clock
                   ; read_address = zero window_size_bits
                   ; read_enable = gnd
                   }
                 ; { read_clock = i.clock
                   ; read_address = i.bucket_address
                   ; read_enable = i.bucket_read_enable
                   }
                |]
              ~write_ports:
                [| { write_clock = i.clock
                   ; write_address = zero window_size_bits
                   ; write_enable = gnd
                   ; write_data = adder
                   }
                |]
              ()
          in
          q.(0), q.(1))
      |> List.unzip
    in
    adder
    <== (ctrl.adder_affine_point +: mux (zero log_num_windows) bucket0
        |> pipeline spec ~n:pipeline_depth);
    { O.done_ = ctrl.done_; bucket = mux (reg spec i.bucket_window) bucket1 }
  ;;
end

module Sim = Cyclesim.With_interface (Model.I) (Model.O)

let ( <-. ) a b = a := Bits.of_int ~width:(Bits.width !a) b

let test () =
  let sim =
    Sim.create
      ~config:Cyclesim.Config.trace_all
      (Model.create
         (Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:false ()))
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
  for i = 0 to 100 do
    for w = 0 to Model.num_windows - 1 do
      inputs.scalar.(w) <-. i + 100
    done;
    inputs.affine_point <-. i;
    Cyclesim.cycle sim
  done;
  inputs.scalar_valid <-. 1;
  waves
;;

let%expect_test "" =
  let waves = test () in
  Waveform.print ~display_height:50 ~display_width:120 ~wave_width:2 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────────────────────────┐
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │i$bucket_address  ││ 0000                                                                                             │
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
    │i$scalar_valid    ││            ┌─────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────┘                                                                                     │
    │i$start           ││      ┌─────┐                                                                                     │
    │                  ││──────┘     └─────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │o$bucket          ││ 0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000│
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │o$done_           ││────────────┐                                                                                     │
    │                  ││            └─────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────┬───────────────────────────────────────────────┬───────────────────────────────│
    │SCLR_CNT          ││ 0000000          │0000001                                        │0000002                        │
    │                  ││──────────────────┴───────────────────────────────────────────────┴───────────────────────────────│
    │                  ││────────────┬─────┬─────────────────────────────────────────┬─────┬───────────────────────────────│
    │STATE             ││ 0          │1    │2                                        │1    │2                              │
    │                  ││────────────┴─────┴─────────────────────────────────────────┴─────┴───────────────────────────────│
    │gnd               ││                                                                                                  │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │vdd               ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││                                                                                                  │
    │                  ││                                                                                                  │
    │                  ││                                                                                                  │
    │                  ││                                                                                                  │
    │                  ││                                                                                                  │
    │                  ││                                                                                                  │
    │                  ││                                                                                                  │
    │                  ││                                                                                                  │
    │                  ││                                                                                                  │
    │                  ││                                                                                                  │
    │                  ││                                                                                                  │
    │                  ││                                                                                                  │
    │                  ││                                                                                                  │
    │                  ││                                                                                                  │
    │                  ││                                                                                                  │
    │                  ││                                                                                                  │
    │                  ││                                                                                                  │
    │                  ││                                                                                                  │
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────────────────────────┘ |}]
;;
