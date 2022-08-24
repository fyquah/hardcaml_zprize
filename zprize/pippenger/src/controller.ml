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
    ignore (pipe_shift.value -- "PIPE_SHIFT" : Signal.t);
    let pipe_shift_next =
      let s = pipe_shift.value in
      lsbs s @: msb s
    in
    let scalar_count = Var.reg spec ~width:log_num_scalars in
    let processing_stalled_points = Var.wire ~default:gnd in
    let processing_scalars = Var.wire ~default:gnd in
    ignore (scalar_count.value -- "SCLR_CNT" : Signal.t);
    let is_in_pipeline = wire 1 in
    let pipes =
      List.init num_windows ~f:(fun window ->
        Bucket_pipeline.hierarchy
          ~window
          scope
          { Bucket_pipeline.I.clock = i.clock
          ; clear = i.clear
          ; scalar_in = mux2 is_in_pipeline (zero window_size_bits) i.scalar.(window)
          ; shift = pipe_shift.value.:(window)
          ; scalar_match = i.scalar.(window)
          })
    in
    is_in_pipeline
    <== mux window.value (List.map pipes ~f:(fun o -> o.is_in_pipeline))
        -- "is_in_pipeline_window";
    let push_stalled = Var.wire ~default:gnd in
    let pop_stalled = Var.wire ~default:gnd in
    let stalled =
      Stalled_point_fifos.hierarchy
        scope
        { Stalled_point_fifos.I.clock = i.clock
        ; clear = i.clear
        ; push = push_stalled.value
        ; scalar
        ; window = window.value
        ; affine_point = i.affine_point
        ; pop = pop_stalled.value
        }
    in
    let bubble = Var.wire ~default:gnd in
    let window_next = window.value +:. 1 in
    let scalar_read = Var.wire ~default:gnd in
    let execute = Var.wire ~default:gnd in
    ignore (sm.current -- "STATE" : Signal.t);
    Always.(
      compile
        [ push_stalled <-- gnd
        ; pop_stalled <-- gnd
        ; pipe_shift <-- pipe_shift_next
        ; sm.switch
            [ ( Start
              , [ scalar_count <--. 0
                ; window <--. 0
                ; pipe_shift <--. 1
                ; when_ i.start [ sm.set_next Choose_mode ]
                ] )
            ; ( Choose_mode
              , [ if_
                    (scalar_count.value ==: ones log_num_scalars)
                    [ sm.set_next Start ]
                    [ scalar_count <-- scalar_count.value +:. 1
                    ; window <-- window_next
                    ; processing_stalled_points <-- gnd
                    ; processing_scalars <-- gnd
                    ; if_
                        (stalled.all_windows_have_stall |: stalled.some_windows_are_full)
                        [ processing_stalled_points <-- vdd
                        ; execute <-- vdd
                        ; sm.set_next Stalled
                        ]
                        [ if_
                            i.scalar_valid
                            [ execute <-- vdd
                            ; processing_scalars <-- vdd
                            ; sm.set_next Scalars
                            ]
                            [ bubble <-- vdd ]
                        ]
                    ]
                ] )
            ; ( Scalars
              , [ window <-- window_next
                ; execute <-- is_in_pipeline
                ; bubble <-- ~:is_in_pipeline
                ; processing_scalars <-- vdd
                ; when_
                    (window.value ==:. num_windows - 1)
                    [ window <--. 0; scalar_read <--. 1; sm.set_next Choose_mode ]
                ] )
            ; ( Stalled
              , [ window <-- window_next
                ; processing_stalled_points <-- vdd
                ; execute <-- vdd
                ; when_
                    (window.value ==:. num_windows - 1)
                    [ window <--. 0; sm.set_next Choose_mode ]
                ] )
            ]
        ]);
    { O.done_ = sm.is Start
    ; scalar_read = scalar_read.value
    ; window = window.value
    ; bucket = mux window.value (Array.to_list i.scalar)
    ; adder_affine_point =
        mux2
          (reg spec processing_stalled_points.value)
          stalled.affine_point_out
          (reg spec i.affine_point)
    ; bubble = bubble.value
    ; execute = execute.value
    }
  ;;

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

     Choose_mode [-> Execute_stalled point or Execute_scalar or Wait_mode or Start]

         - check if stalled fifo is ready, check if there is an available scalar
 
     Wait_mode [-> Choose_mode]

     Execute_stalled_point [-> Wait_stalled_point or Choose_mode]

     Wait_stalled_point [-> Execute_stalled_point]

     Execute_scalar [-> Wait_scalar or Choose_mode]

     Wait_scalar [-> Execute_scalar]

  *)

  module State2 = struct
    type t =
      | Start
      | Choose_mode
      | Wait_mode
      | Execute_scalar
      | Wait_scalar
      | Execute_stalled
      | Wait_stalled
    [@@deriving sexp_of, compare, enumerate]
  end

  let create2 scope (i : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let stalled = Stalled_point_fifos.O.Of_signal.wires () in
    let pipes =
      List.init num_windows ~f:(fun _ -> Bucket_pipeline.O.Of_signal.wires ())
    in
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = Always.State_machine.create (module State2) spec in
    let window = Var.reg spec ~width:log_num_windows in
    let window_next = window.value +:. 1 in
    let scalar = mux window.value (Array.to_list i.scalar) in
    let scalar_count = Var.reg spec ~width:log_num_scalars in
    let scalar_count_next = scalar_count.value +:. 1 in
    ignore (sm.current -- "STATE" : Signal.t);
    let on_last_window =
      Always.(
        when_
          (window.value ==:. num_windows - 1)
          [ window <--. 0; scalar_count <-- scalar_count_next; sm.set_next Choose_mode ])
    in
    Always.(
      compile
        [ sm.switch
            [ Start, [ window <--. 0; when_ i.start [ sm.set_next Choose_mode ] ]
            ; ( Choose_mode
              , [ if_
                    (stalled.all_windows_have_stall |: stalled.some_windows_are_full)
                    [ (* process stalled coefficients in each window *)
                      sm.set_next Execute_stalled
                    ]
                  @@ elif
                       i.scalar_valid
                       [ (* process an input scalar across all windows *)
                         sm.set_next Execute_scalar
                       ]
                       [ (* nothing to do - wait and try again *) sm.set_next Wait_mode ]
                ] )
            ; Wait_mode, [ sm.set_next Choose_mode ]
            ; Execute_scalar, [ sm.set_next Wait_scalar ]
            ; ( Wait_scalar
              , [ window <-- window_next; sm.set_next Execute_scalar; on_last_window ] )
            ; Execute_stalled, [ sm.set_next Wait_stalled ]
            ; ( Wait_stalled
              , [ window <-- window_next; sm.set_next Execute_stalled; on_last_window ] )
            ]
        ]);
    List.iter2_exn
      pipes
      (List.init num_windows ~f:(fun window ->
         Bucket_pipeline.hierarchy
           ~window
           scope
           { Bucket_pipeline.I.clock = i.clock
           ; clear = i.clear
           ; scalar_in = mux2 is_in_pipeline (zero window_size_bits) i.scalar.(window)
           ; shift = gnd
           ; scalar_match = i.scalar.(window)
           }))
      ~f:Bucket_pipeline.O.Of_signal.assign;
    Stalled_point_fifos.O.Of_signal.assign
      stalled
      (Stalled_point_fifos.hierarchy
         scope
         { Stalled_point_fifos.I.clock = i.clock
         ; clear = i.clear
         ; push = gnd
         ; scalar
         ; window = window.value
         ; affine_point = i.affine_point
         ; pop = gnd
         });
    O.Of_signal.of_int 0
  ;;

  let hierarchy scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~name:"ctrl" ~scope create
  ;;
end
