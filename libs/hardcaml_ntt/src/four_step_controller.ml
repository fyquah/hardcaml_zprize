open Base
open Hardcaml
open Signal

module Make (Config : Core_config.S) = struct
  open Config

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; input_done : 'a
      ; output_done : 'a
      ; cores_done : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { done_ : 'a
      ; start_input : 'a
      ; start_output : 'a
      ; start_cores : 'a
      ; first_iter : 'a
      ; flip : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module State = struct
    type t =
      | Start
      | First_load
      | Main_iter
      | Last_store
      | Finish
    [@@deriving sexp_of, compare, enumerate]
  end

  module Var = Always.Variable

  (* We need to track 3 external states


       1. Input ready
       2. Core ready
       3. Output ready

       When each of these occurs, we are ready to start a new set of transforms, and
       request that the IO subsystem run.

    *)

  let create scope (i : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = Always.State_machine.create (module State) spec in
    let start_input = Var.wire ~default:gnd in
    let start_output = Var.wire ~default:gnd in
    let start_cores = Var.wire ~default:gnd in
    let first_iter = Var.wire ~default:gnd in
    let log_num_iterations = logn - logcores - logblocks in
    let iteration = Var.reg spec ~width:(max 1 log_num_iterations) in
    let iteration_next = iteration.value +:. 1 in
    ignore (sm.current -- "STATE");
    ignore (start_input.value -- "START_INPUT");
    ignore (start_output.value -- "START_OUTPUT");
    ignore (start_cores.value -- "START_CORES");
    ignore (iteration.value -- "ITERATION");
    let all_done = i.input_done &: i.output_done &: i.cores_done in
    Always.(
      compile
        [ sm.switch
            [ Start, [ when_ i.start [ start_input <-- vdd; sm.set_next First_load ] ]
            ; ( First_load
              , [ when_
                    all_done
                    [ iteration <--. 1
                    ; (start_input <-- if log_num_iterations <> 0 then vdd else gnd)
                    ; start_cores <-- vdd
                    ; first_iter <--. 1
                    ; (if log_num_iterations <> 0
                      then sm.set_next Main_iter
                      else sm.set_next Last_store)
                    ]
                ] )
            ; ( Main_iter
              , [ when_
                    all_done
                    [ iteration <-- iteration_next
                    ; if_
                        (iteration_next ==:. 0)
                        [ start_output <-- vdd
                        ; start_cores <-- vdd
                        ; sm.set_next Last_store
                        ]
                        [ start_input <-- vdd; start_output <-- vdd; start_cores <-- vdd ]
                    ]
                ] )
            ; Last_store, [ when_ all_done [ start_output <-- vdd; sm.set_next Finish ] ]
            ; Finish, [ when_ all_done [ sm.set_next Start ] ]
            ]
        ]);
    { O.done_ = sm.is Start
    ; start_input = start_input.value
    ; start_output = start_output.value
    ; start_cores = start_cores.value
    ; first_iter = first_iter.value
    ; flip = start_cores.value |: start_output.value
    }
  ;;

  let hierarchy scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~name:"controller" ~scope create
  ;;
end
