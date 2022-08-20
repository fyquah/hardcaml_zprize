open! Base
open! Hardcaml

module type Config = sig
  val window_size_bits : int
  val num_windows : int
  val affine_point_bits : int
  val pipeline_depth : int
end

module Make (Config : Config) = struct
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

    module O = struct
      type 'a t = { matches : 'a } [@@deriving sexp_of, hardcaml]
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
      | P0
      | P1
    [@@deriving sexp_of, compare, enumerate]
  end

  module Var = Always.Variable

  let create scope (i : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = Always.State_machine.create (module State) spec in
    let window = Var.reg spec ~width:log_num_windows in
    let pipe_shift = Var.reg spec ~width:num_windows in
    let _pipes =
      Array.init num_windows ~f:(fun index ->
          Bucket_pipeline_model.create
            scope
            { Bucket_pipeline_model.I.clock = i.clock
            ; clear = i.clear
            ; scalar_in = i.scalar.(index)
            ; shift = pipe_shift.value.:(index)
            ; scalar_match = zero window_size_bits
            })
    in
    ignore (sm.current -- "STATE" : Signal.t);
    Always.(
      compile
        [ pipe_shift <--. 0
        ; sm.switch
            [ Start, [ window <--. 0; when_ i.start [ sm.set_next P0 ] ]
            ; P0, [ when_ i.scalar_valid [ sm.set_next P0 ] ]
            ; P1, [ sm.set_next P1 ]
            ]
        ]);
    { O.done_ = sm.is Start
    ; scalar_read = gnd
    ; bucket = zero window_size_bits
    ; window = window.value
    ; adder_affine_point = zero affine_point_bits
    ; bubble = gnd
    ; execute = gnd
    }
  ;;
end
