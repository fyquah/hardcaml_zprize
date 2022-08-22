open! Base
open! Hardcaml

(* XX aray: For the initial version, we are going to make a slightely simpler design
   which runs at coefficient per cycle.  We practically require 2 cycles.

   We will have to fix this.  If the controller is fast enough anyway, we could do
   this with an enable and job done.  If not, then I think extending the design should be fairly easy.
*)

module Make (Config : Config.S) = struct
  open Signal
  open Config

  let pipeline_depth_per_window = (pipeline_depth + num_windows - 1) / num_windows
  let log_num_windows = Int.ceil_log2 num_windows

  module Bucket_pipeline_model = struct
    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; scalar_in : 'a [@bits window_size_bits]
        ; shift : 'a
        ; scalar_match : 'a [@bits window_size_bits]
        }
      [@@deriving sexp_of, hardcaml]
    end

    let create _scope (i : _ I.t) =
      let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
      let rec build_pipe n d pipe =
        if n = pipeline_depth_per_window
        then pipe
        else (
          let d = reg spec ~enable:i.shift d in
          build_pipe (n + 1) d (d :: pipe))
      in
      let stored = build_pipe 0 i.scalar_in [] in
      List.map stored ~f:(fun d -> d ==: i.scalar_match)
      |> tree ~arity:6 ~f:(reduce ~f:( |: ))
    ;;
  end

  module Stalled_point_fifo = Stalled_point_fifo.Make (Config)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; scalar : 'a array [@bits window_size_bits] [@length num_windows]
      ; scalar_valid : 'a
      ; affine_point : 'a [@bits affine_point_bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { done_ : 'a
      ; scalar_read : 'a
      ; bucket : 'a [@bits window_size_bits]
      ; window : 'a [@bits log_num_windows]
      ; adder_affine_point : 'a [@bits affine_point_bits]
      ; bubble : 'a
      ; execute : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module State = struct
    type t =
      | Start
      | Choose_mode
      | Scalars
      | Stalled
    [@@deriving sexp_of, compare, enumerate]
  end

  module Var = Always.Variable

  let create scope (i : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = Always.State_machine.create (module State) spec in
    let window = Var.reg spec ~width:log_num_windows in
    let scalar = mux window.value (Array.to_list i.scalar) in
    let pipe_shift = Var.reg spec ~width:num_windows in
    let scalar_count = Var.reg spec ~width:log_num_scalars in
    ignore (scalar_count.value -- "SCLR_CNT" : Signal.t);
    let _pipes =
      List.init num_windows ~f:(fun index ->
          Bucket_pipeline_model.create
            scope
            { Bucket_pipeline_model.I.clock = i.clock
            ; clear = i.clear
            ; scalar_in = i.scalar.(index)
            ; shift = pipe_shift.value.:(index)
            ; scalar_match = zero window_size_bits
            })
    in
    let push_stalled = Var.wire ~default:gnd in
    let pop_stalled = Var.wire ~default:gnd in
    let stalled =
      Stalled_point_fifo.hierarchy
        scope
        { Stalled_point_fifo.I.clock = i.clock
        ; clear = i.clear
        ; push = push_stalled.value
        ; scalar
        ; window = window.value
        ; affine_point = i.affine_point
        ; pop = pop_stalled.value
        }
    in
    let processing_stalled_points = Var.reg spec ~width:1 in
    let bubble = Var.wire ~default:gnd in
    ignore (sm.current -- "STATE" : Signal.t);
    Always.(
      compile
        [ pipe_shift <--. 0
        ; (* XXX *)
          push_stalled <-- gnd
        ; pop_stalled <-- gnd
        ; sm.switch
            [ ( Start
              , [ scalar_count <--. 0
                ; window <--. 0
                ; when_ i.start [ sm.set_next Choose_mode ]
                ] )
            ; ( Choose_mode
              , [ if_
                    (scalar_count.value ==: ones log_num_scalars)
                    [ sm.set_next Start ]
                    [ scalar_count <-- scalar_count.value +:. 1
                    ; when_
                        i.scalar_valid
                        [ if_
                            (stalled.all_windows_have_stall
                            |: stalled.some_windows_are_full)
                            [ processing_stalled_points <-- vdd; sm.set_next Stalled ]
                            [ if_
                                i.scalar_valid
                                [ processing_stalled_points <-- gnd; sm.set_next Scalars ]
                                [ bubble <-- vdd ]
                            ]
                        ]
                    ]
                ] )
            ; ( Scalars
              , [ window <-- window.value +:. 1
                ; when_
                    (window.value ==:. num_windows - 1)
                    [ window <--. 0; sm.set_next Choose_mode ]
                ] )
            ; ( Stalled
              , [ window <-- window.value +:. 1
                ; when_
                    (window.value ==:. num_windows - 1)
                    [ window <--. 0; sm.set_next Choose_mode ]
                ] )
            ]
        ]);
    { O.done_ = sm.is Start
    ; scalar_read = gnd
    ; bucket = zero window_size_bits
    ; window = window.value
    ; adder_affine_point = zero affine_point_bits
    ; bubble = bubble.value
    ; execute = gnd
    }
  ;;
end
