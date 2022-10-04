open! Base
open! Hardcaml

module Make (Config : Config.S) (Scalar_config : Scalar.Scalar_config.S) = struct
  open Signal
  open Config
  module Stalled_point_fifos = Stalled_point_fifos.Make (Config) (Scalar_config)
  module Track_scalars = Track_scalars.Make (Config) (Scalar_config)
  module Scalar = Scalar.Make (Scalar_config)

  let log_num_windows = Int.ceil_log2 num_windows

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; scalar : 'a Scalar.t array [@length num_windows] [@rtlmangle true]
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
      ; bucket : 'a Scalar.t [@rtlmangle true]
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
      | Wait_bubble
      | Execute_scalar
      | Wait_scalar
      | Execute_stalled
      | Wait_stalled
    [@@deriving sexp_of, compare, enumerate]

    let names =
      List.map all ~f:(function
        | Start -> "-"
        | Choose_mode -> "M"
        | Bubble_mode -> "B?"
        | Wait_bubble -> "W?"
        | Execute_scalar -> "E+"
        | Wait_scalar -> "W+"
        | Execute_stalled -> "E-"
        | Wait_stalled -> "W-")
    ;;
  end

  (* This implements [mux window data] on the 2 cycle schedule of the
     controller.

     It builds a shift pipeline and outputs the first value combinationally. So,
     rather than an N deep mux, it is 2 deep. *)
  let latched_shift_array spec ~load ~shift data : _ Scalar.t =
    let len = Array.length data in
    let rec shift_regs n =
      if n = len
      then Scalar.Of_signal.of_int 0
      else
        Scalar.map
          ~f:(reg spec ~enable:(load |: shift))
          (Scalar.Of_signal.mux2 load data.(n) (shift_regs (n + 1)))
    in
    Scalar.Of_signal.mux2 load data.(0) (shift_regs 0)
  ;;

  let create ~build_mode scope (i : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let stalled = Stalled_point_fifos.O.Of_signal.wires () in
    let tracker = Track_scalars.O.Of_signal.wires () in
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = Always.State_machine.create (module State) spec in
    ignore (sm.current -- "STATE" : Signal.t);
    let window = Var.reg spec ~width:log_num_windows in
    let window_next = window.value +:. 1 in
    let latch = Var.wire ~default:gnd in
    let shift_pipeline = Var.wire ~default:gnd in
    let scalar, scalar_is_zero =
      let scalar =
        latched_shift_array spec ~load:latch.value ~shift:shift_pipeline.value i.scalar
      in
      scalar, reg spec (scalar.scalar ==:. 0)
    in
    let stalled_scalar, stalled_scalar_is_zero =
      let scalar =
        latched_shift_array
          spec
          ~load:latch.value
          ~shift:shift_pipeline.value
          stalled.scalars_out
      in
      scalar, reg spec (scalar.scalar ==:. 0)
    in
    let bubble = Var.wire ~default:gnd in
    let push_stalled_point = Var.wire ~default:gnd in
    let pop_stalled_point = Var.wire ~default:gnd in
    let is_in_pipeline = reg spec tracker.is_in_pipeline in
    let scalar_read = Var.reg spec ~width:1 in
    let flushing = Var.reg spec ~width:1 in
    ignore (flushing.value -- "flushing" : Signal.t);
    let on_last_window ~processing_scalars =
      Always.(
        when_
          (window.value ==:. num_windows - 1)
          [ window <--. 0
          ; sm.set_next Choose_mode
          ; (if processing_scalars then proc [ flushing <-- i.last_scalar ] else proc [])
          ])
    in
    let maybe_scalar_read =
      Always.(when_ (window.value ==:. num_windows - 1) [ scalar_read <-- vdd ])
    in
    let executing_stalled = Var.wire ~default:gnd in
    let executing_scalar = Var.wire ~default:gnd in
    Always.(
      compile
        [ scalar_read <-- gnd
        ; sm.switch
            [ ( Start
              , [ flushing <-- gnd
                ; window <--. 0
                ; when_ i.start [ sm.set_next Choose_mode ]
                ] )
            ; ( Choose_mode
              , [ (* latch the pipeline control at the start.  This tracks values
                   for a few cycles longer than necesasry, but reduces the critical path *)
                  latch <--. 1
                ; if_
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
              , [ bubble <-- vdd
                ; shift_pipeline <-- vdd
                ; window <-- window_next
                ; sm.set_next Wait_bubble
                ; on_last_window ~processing_scalars:false
                ] )
            ; Wait_bubble, [ sm.set_next Bubble_mode ]
            ; ( Execute_scalar
              , [ executing_scalar <-- vdd
                ; shift_pipeline <-- vdd
                ; bubble <-- (is_in_pipeline |: scalar_is_zero)
                ; push_stalled_point <-- (is_in_pipeline &: ~:scalar_is_zero)
                ; window <-- window_next
                ; sm.set_next Wait_scalar
                ; on_last_window ~processing_scalars:true
                ] )
            ; ( Wait_scalar
              , [ executing_scalar <-- vdd
                ; maybe_scalar_read
                ; sm.set_next Execute_scalar
                ] )
            ; ( Execute_stalled
              , [ executing_stalled <-- vdd
                ; shift_pipeline <-- vdd
                ; bubble
                  <-- (is_in_pipeline
                      |: (stalled.scalar_out_valid &: stalled_scalar_is_zero)
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
    Stalled_point_fifos.O.Of_signal.assign
      stalled
      (Stalled_point_fifos.hierarchy
         ~build_mode
         scope
         { Stalled_point_fifos.I.clock = i.clock
         ; clear = i.clear
         ; push = push_stalled_point.value -- "push"
         ; scalar
         ; window = window.value
         ; affine_point = i.affine_point
         ; pop = pop_stalled_point.value -- "pop"
         });
    Track_scalars.O.Of_signal.assign
      tracker
      (Track_scalars.hierarchy
         scope
         { Track_scalars.I.clock = i.clock
         ; scalar = Scalar.Of_signal.mux2 executing_stalled stalled_scalar scalar
         ; bubble = is_in_pipeline
         ; shift = shift_pipeline.value
         });
    { O.done_ = sm.is Start
    ; scalar_read = scalar_read.value
    ; window = window.value
    ; bucket = Scalar.Of_signal.mux2 executing_scalar scalar stalled.scalar_out
    ; adder_affine_point = mux2 executing_scalar i.affine_point stalled.affine_point_out
    ; bubble = bubble.value
    ; execute = shift_pipeline.value
    }
  ;;

  let hierarchy ~build_mode scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~name:"ctrl" ~scope (create ~build_mode)
  ;;

  module For_synthesis = struct
    let create scope (i : _ I.t) =
      let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
      hierarchy
        ~build_mode:Synthesis
        scope
        { (I.map i ~f:(reg spec)) with clock = i.clock; clear = i.clear }
      |> O.map ~f:(reg spec)
    ;;
  end
end
