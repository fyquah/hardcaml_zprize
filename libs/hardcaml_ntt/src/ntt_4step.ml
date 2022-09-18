open Base
open Hardcaml
open Signal

module type Config = sig
  include Ntt.Config

  val logcores : int
end

module Make (Config : Config) = struct
  include Config

  let () =
    if Config.logn < 4
    then
      raise_s
        [%message
          "Minimum logn for 4step algorithm is 4 (256 point total)" (Config.logn : int)]
  ;;

  let cores = 1 lsl logcores

  module Gf = Gf.Signal
  module Ntt = Ntt.Make (Config)

  module Axi_stream = Hardcaml_axi.Stream.Make (struct
    let addr_bits = 32
    let data_bits = Gf.num_bits * cores
  end)

  module Parallel_cores = struct
    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; start : 'a
        ; first_4step_pass : 'a
        ; first_iter : 'a
        ; flip : 'a
        ; wr_d : 'a array [@bits Gf.num_bits] [@length cores]
        ; wr_en : 'a [@bits cores]
        ; wr_addr : 'a [@bits logn]
        ; rd_en : 'a [@bits cores]
        ; rd_addr : 'a [@bits logn]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t =
        { done_ : 'a
        ; rd_q : 'a array [@bits Gf.num_bits] [@length cores]
        }
      [@@deriving sexp_of, hardcaml]
    end

    let create ~build_mode scope (i : _ I.t) =
      let cores =
        Array.init cores ~f:(fun index ->
          Ntt.With_rams.hierarchy
            ~row:index
            ~build_mode
            ~instance:("ntt" ^ Int.to_string index)
            scope
            { Ntt.With_rams.I.clock = i.clock
            ; clear = i.clear
            ; start = i.start
            ; first_iter = i.first_iter
            ; first_4step_pass = i.first_4step_pass
            ; flip = i.flip
            ; wr_d = i.wr_d.(index)
            ; wr_en = i.wr_en.:(index)
            ; wr_addr = i.wr_addr
            ; rd_en = i.rd_en.:(index)
            ; rd_addr = i.rd_addr
            })
      in
      { O.done_ = cores.(0).done_; rd_q = Array.map cores ~f:(fun core -> core.rd_q) }
    ;;

    let hierarchy ~build_mode scope =
      let module Hier = Hierarchy.In_scope (I) (O) in
      Hier.hierarchical ~name:"parallel_cores" ~scope (create ~build_mode)
    ;;
  end

  module Controller = struct
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
      let log_num_iterations = logn - logcores in
      let iteration = Var.reg spec ~width:log_num_iterations in
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
                      ; start_input <-- vdd
                      ; start_cores <-- vdd
                      ; first_iter <--. 1
                      ; sm.set_next Main_iter
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
                          [ start_input <-- vdd
                          ; start_output <-- vdd
                          ; start_cores <-- vdd
                          ]
                      ]
                  ] )
              ; ( Last_store
                , [ when_ all_done [ start_output <-- vdd; sm.set_next Finish ] ] )
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

  module Core = struct
    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; start : 'a
        ; first_4step_pass : 'a
        ; wr_d : 'a array [@bits Gf.num_bits] [@length cores]
        ; wr_en : 'a [@bits cores]
        ; wr_addr : 'a [@bits logn]
        ; rd_en : 'a [@bits cores]
        ; rd_addr : 'a [@bits logn]
        ; input_done : 'a
        ; output_done : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t =
        { done_ : 'a
        ; start_input : 'a
        ; start_output : 'a
        ; rd_q : 'a array [@bits Gf.num_bits] [@length cores]
        }
      [@@deriving sexp_of, hardcaml]
    end

    let create ~build_mode scope (i : _ I.t) =
      let cores_done = wire 1 in
      let controller =
        Controller.hierarchy
          scope
          { Controller.I.clock = i.clock
          ; clear = i.clear
          ; start = i.start
          ; input_done = i.input_done
          ; output_done = i.output_done
          ; cores_done
          }
      in
      let cores =
        Parallel_cores.hierarchy
          ~build_mode
          scope
          { Parallel_cores.I.clock = i.clock
          ; clear = i.clear
          ; start = controller.start_cores
          ; first_iter = controller.first_iter
          ; first_4step_pass = i.first_4step_pass
          ; flip = controller.flip
          ; wr_d = i.wr_d
          ; wr_en = i.wr_en
          ; wr_addr = i.wr_addr
          ; rd_en = i.rd_en
          ; rd_addr = i.rd_addr
          }
      in
      cores_done <== cores.done_;
      { O.done_ = controller.done_
      ; start_input = controller.start_input
      ; start_output = controller.start_output
      ; rd_q = cores.rd_q
      }
    ;;

    let hierarchy ~build_mode scope =
      let module Hier = Hierarchy.In_scope (I) (O) in
      Hier.hierarchical ~name:"cores" ~scope (create ~build_mode)
    ;;
  end
end
