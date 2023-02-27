open! Base
open! Hardcaml

module Make (Config : Config.S) (Scalar : Scalar_element.S) = struct
  open! Signal
  open Config
  module Track_scalars = Track_scalars.Make (Config) (Scalar)

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

  module State = struct
    type t =
      | Start
      | Run
    [@@deriving sexp_of, compare, enumerate]

    let names =
      List.map all ~f:(function
          | Start -> "-"
          | Run -> "R")
    ;;
  end

  let create ~build_mode:_ scope (i : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = Always.State_machine.create (module State) spec in
    ignore (sm.current -- "STATE" : Signal.t);
    let tracker = Track_scalars.O.Of_signal.wires () in
    Always.(
      compile
        [ sm.switch
            [ Start, [ when_ i.start [ sm.set_next Run ] ]
            ; Run, [ when_ i.scalar_valid [ when_ i.last_scalar [] ] ]
            ]
        ]);
    Track_scalars.O.Of_signal.assign
      tracker
      (Track_scalars.hierarchy
         scope
         { Track_scalars.I.clock = i.clock
         ; scalar = Scalar.Of_signal.of_int 0
         ; bubble = gnd
         ; shift = gnd
         });
    O.Of_signal.of_int 0
  ;;

  let hierarchy ~build_mode scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~name:"ctrl_fr" ~scope (create ~build_mode)
  ;;

  module For_synthesis = struct
    let create scope (i : _ I.t) =
      let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
      hierarchy
        ~build_mode:Build_mode.Synthesis
        scope
        { (I.map i ~f:(reg spec)) with clock = i.clock; clear = i.clear }
      |> O.map ~f:(reg spec)
    ;;
  end
end
