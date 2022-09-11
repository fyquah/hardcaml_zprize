open! Base
open! Hardcaml

module Make (Config : Config.S) = struct
  open Signal
  open Config
  module Bucket_pipeline = Bucket_pipeline.Make (Config)

  let log_num_windows = Int.ceil_log2 num_windows

  module Stalled_point_fifos = Stalled_point_fifos.Make (Config)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; scalar : 'a array [@bits window_size_bits] [@length num_windows]
      ; scalar_valid : 'a
      ; last_scalar : 'a
      ; affine_point : 'a [@bits affine_point_bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { done_ : 'a
      ; scalar_read : 'a
      ; window : 'a [@bits log_num_windows]
      ; bucket : 'a [@bits window_size_bits]
      ; adder_affine_point : 'a [@bits affine_point_bits]
      ; bubble : 'a
      ; execute : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Var = Always.Variable

  (* We can issue every other cycle. There are a few things we need to check/calculate.

     1. Have we got stalled points to execute? Go to 2.1
        Are scalars ready? Go to 2.2
        Otherwise - go to 2.3

     2.1 Execute stalled points - 1st starts on this cycle, then complete all windows

     2.2 Execute Scalars - 1st starts on this cycle, then complete all windows

     2.3 1 cycle bubble - go to 1

     States:
     -------------------------------------

     Start [-> Choose_mode]

     Choose_mode [-> Execute_stalled point or Execute_scalar or Bubble_mode or Start]

         - check if stalled fifo is ready, check if there is an available scalar
 
     Bubble_mode [-> Choose_mode]

     Execute_stalled_point [-> Wait_stalled_point or Choose_mode]

     Wait_stalled_point [-> Execute_stalled_point]

     Execute_scalar [-> Wait_scalar or Choose_mode]

     Wait_scalar [-> Execute_scalar]

  *)

  module State = struct
    type t =
      | Start
      | Choose_mode
      | Bubble_mode
      | Execute_scalar
      | Wait_scalar
      | Execute_stalled
      | Wait_stalled
    [@@deriving sexp_of, compare, enumerate]

    let names =
      List.map all ~f:(function
          | Start -> "-"
          | Choose_mode -> "M"
          | Bubble_mode -> "Mb"
          | Execute_scalar -> "Es"
          | Wait_scalar -> "Ws"
          | Execute_stalled -> "E-"
          | Wait_stalled -> "W-")
    ;;
  end

  let create scope (i : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let stalled = Stalled_point_fifos.O.Of_signal.wires () in
    let pipes = Bucket_pipeline.O.Of_signal.wires () in
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = Always.State_machine.create (module State) spec in
    let window = Var.reg spec ~width:log_num_windows in
    let window_next = window.value +:. 1 in
    let scalar = mux window.value (Array.to_list i.scalar) in
    let bubble = Var.wire ~default:gnd in
    let push_stalled_point = Var.wire ~default:gnd in
    let pop_stalled_point = Var.wire ~default:gnd in
    let is_in_pipeline = mux window.value pipes.is_in_pipeline -- "is_in_pipeline" in
    let shift_pipeline = Var.wire ~default:gnd in
    let scalar_read = Var.wire ~default:gnd in
    let flushing = Var.reg spec ~width:1 in
    ignore (sm.current -- "STATE" : Signal.t);
    let on_last_window ~processing_scalars =
      Always.(
        when_
          (window.value ==:. num_windows - 1)
          [ window <--. 0
          ; sm.set_next Choose_mode
          ; (if processing_scalars
            then proc [ scalar_read <-- vdd; flushing <-- i.last_scalar ]
            else scalar_read <-- gnd)
          ])
    in
    let executing_stalled = Var.wire ~default:gnd in
    let executing_scalar = Var.wire ~default:gnd in
    Always.(
      compile
        [ sm.switch
            [ ( Start
              , [ flushing <-- gnd
                ; window <--. 0
                ; when_ i.start [ sm.set_next Choose_mode ]
                ] )
            ; ( Choose_mode
              , [ if_
                    flushing.value
                    [ sm.set_next Execute_stalled
                    ; executing_stalled <-- vdd
                    ; when_ stalled.all_windows_are_empty [ sm.set_next Start ]
                    ]
                  @@ elif
                       (stalled.all_windows_have_stall |: stalled.some_windows_are_full)
                       [ (* process stalled coefficients in each window *)
                         executing_stalled <-- vdd
                       ; sm.set_next Execute_stalled
                       ]
                  @@ elif
                       i.scalar_valid
                       [ (* process an input scalar across all windows *)
                         executing_scalar <-- vdd
                       ; sm.set_next Execute_scalar
                       ]
                       [ (* nothing to do - wait and try again *)
                         sm.set_next Bubble_mode
                       ]
                ] )
            ; ( Bubble_mode
              , [ bubble <-- vdd; shift_pipeline <-- vdd; sm.set_next Choose_mode ] )
            ; ( Execute_scalar
              , [ executing_scalar <-- vdd
                ; shift_pipeline <-- vdd
                ; bubble <-- (is_in_pipeline |: (scalar ==:. 0))
                ; push_stalled_point <-- (is_in_pipeline &: (scalar <>:. 0))
                ; window <-- window_next
                ; sm.set_next Wait_scalar
                ; on_last_window ~processing_scalars:true
                ] )
            ; Wait_scalar, [ executing_scalar <-- vdd; sm.set_next Execute_scalar ]
            ; ( Execute_stalled
              , [ executing_stalled <-- vdd
                ; shift_pipeline <-- vdd
                ; bubble
                  <-- (is_in_pipeline
                      |: (stalled.scalar_out ==:. 0)
                      |: ~:(stalled.current_window_has_stall))
                ; pop_stalled_point <-- ~:(bubble.value)
                ; window <-- window_next
                ; sm.set_next Wait_stalled
                ; on_last_window ~processing_scalars:false
                ] )
            ; Wait_stalled, [ executing_stalled <-- vdd; sm.set_next Execute_stalled ]
            ]
        ]);
    let executing_stalled = executing_stalled.value -- "exec_stalled" in
    let executing_scalar = executing_scalar.value -- "exec_scalar" in
    Bucket_pipeline.O.Of_signal.assign
      pipes
      (Bucket_pipeline.hierarchy
         scope
         { Bucket_pipeline.I.clock = i.clock
         ; clear = i.clear
         ; window = window.value
         ; scalar_in = i.scalar
         ; stalled_scalar = stalled.scalar_out
         ; process_stalled = executing_stalled
         ; bubble = is_in_pipeline
         ; shift = shift_pipeline.value
         });
    Stalled_point_fifos.O.Of_signal.assign
      stalled
      (Stalled_point_fifos.hierarchy
         scope
         { Stalled_point_fifos.I.clock = i.clock
         ; clear = i.clear
         ; push = push_stalled_point.value -- "push"
         ; scalar
         ; window = window.value
         ; affine_point = i.affine_point
         ; pop = pop_stalled_point.value -- "pop"
         });
    { O.done_ = sm.is Start
    ; scalar_read = scalar_read.value
    ; window = window.value
    ; bucket = mux2 executing_scalar scalar stalled.scalar_out
    ; adder_affine_point = mux2 executing_scalar i.affine_point stalled.affine_point_out
    ; bubble = bubble.value
    ; execute = shift_pipeline.value
    }
  ;;

  let hierarchy scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~name:"ctrl" ~scope create
  ;;
end
