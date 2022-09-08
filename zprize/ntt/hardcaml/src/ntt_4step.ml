open Base
open Hardcaml
open Signal

module Make (Config : Ntt.Config) = struct
  open Config

  let () =
    if Config.logn < 4
    then
      raise_s
        [%message
          "Minimum logn for 4step algorithm is 4 (256 point total)" (Config.logn : int)]
  ;;

  (* Specifying this as a log is probably not as flexible as we want (if we can
   fit 29 cores, this would limit us to 16). But it greatly simplifies things to
   start with. *)
  let logcores = 3
  let cores = 1 lsl logcores

  module Gf = Gf_bits.Make (Hardcaml.Signal)
  module Ntt = Ntt.Make (Config)

  module Parallel_cores = struct
    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; start : 'a
        ; first_4step_pass : 'a
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

  module Kernel = struct
    module Axi512 = Hardcaml_axi.Axi512

    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; start : 'a
        ; first_4step_pass : 'a
        ; data_in : 'a Axi512.Stream.Source.t
        ; data_out_dest : 'a Axi512.Stream.Dest.t
        }
      [@@deriving sexp_of, hardcaml ~rtlmangle:true]
    end

    module O = struct
      type 'a t =
        { data_out : 'a Axi512.Stream.Source.t
        ; data_in_dest : 'a Axi512.Stream.Dest.t
        ; done_ : 'a
        }
      [@@deriving sexp_of, hardcaml ~rtlmangle:true]
    end

    module Var = Always.Variable

    module Load_sm = struct
      module State = struct
        type t =
          | Start
          | Stream
        [@@deriving sexp_of, compare, enumerate]
      end

      type 'a t =
        { done_ : 'a
        ; tready : 'a
        ; wr_addr : 'a
        ; wr_en : 'a
        }
      [@@deriving sexp_of, hardcaml]

      let create (i : _ I.t) ~start =
        let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
        let sm = Always.State_machine.create (module State) spec in
        let addr = Var.reg spec ~width:logn in
        let addr_next = addr.value +:. 1 in
        Always.(
          compile
            [ sm.switch
                [ Start, [ addr <--. 0; when_ start [ sm.set_next Stream ] ]
                ; ( Stream
                  , [ when_
                        i.data_in.tvalid
                        [ addr <-- addr_next
                        ; when_ (addr_next ==:. 0) [ sm.set_next Start ]
                        ]
                    ] )
                ]
            ]);
        let done_ = sm.is Start in
        let processing = ~:done_ in
        { done_
        ; tready = processing
        ; wr_en = processing &: i.data_in.tvalid
        ; wr_addr = addr.value
        }
      ;;
    end

    module Store_sm = struct
      module State = struct
        type t =
          | Start
          | Preroll
          | Stream
        [@@deriving sexp_of, compare, enumerate]
      end

      type 'a t =
        { done_ : 'a
        ; tvalid : 'a
        ; rd_addr : 'a
        ; rd_en : 'a
        }
      [@@deriving sexp_of, hardcaml]

      let create (i : _ I.t) ~start =
        let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
        let sm = Always.State_machine.create (module State) spec in
        let addr = Var.reg spec ~width:(logn + 1) in
        let addr_next = addr.value +:. 1 in
        let rd_en = Var.wire ~default:gnd in
        let tvalid = Var.reg spec ~width:1 in
        (* XXX fixme *)
        Always.(
          compile
            [ sm.switch
                [ Start, [ addr <--. 0; when_ start [ sm.set_next Preroll ] ]
                ; ( Preroll
                  , [ rd_en <-- vdd; addr <--. 1; tvalid <-- vdd; sm.set_next Stream ] )
                ; ( Stream
                  , [ when_
                        i.data_out_dest.tready
                        [ addr <-- addr_next
                        ; rd_en <-- vdd
                        ; when_
                            (addr.value ==:. 1 lsl logn)
                            [ tvalid <-- gnd; sm.set_next Start ]
                        ]
                    ] )
                ]
            ]);
        let done_ = sm.is Start in
        { done_
        ; tvalid = tvalid.value
        ; rd_addr = addr.value.:[logn - 1, 0]
        ; rd_en = rd_en.value
        }
      ;;
    end

    let create ~build_mode scope (i : _ I.t) =
      let start_input = wire 1 in
      let start_output = wire 1 in
      let load_sm = Load_sm.create i ~start:start_input in
      let store_sm = Store_sm.create i ~start:start_output in
      let cores =
        Core.create
          ~build_mode
          scope
          { Core.I.clock = i.clock
          ; clear = i.clear
          ; start = i.start
          ; first_4step_pass = i.first_4step_pass
          ; wr_d = i.data_in.tdata |> split_lsb ~part_width:Gf.num_bits |> Array.of_list
          ; wr_en = repeat (i.data_in.tvalid &: load_sm.tready) cores
          ; wr_addr = load_sm.wr_addr
          ; rd_en = repeat store_sm.rd_en cores
          ; rd_addr = store_sm.rd_addr
          ; input_done = load_sm.done_
          ; output_done = store_sm.done_
          }
      in
      start_input <== cores.start_input;
      start_output <== cores.start_output;
      { O.data_out =
          { tvalid = store_sm.tvalid
          ; tdata = cores.rd_q |> Array.to_list |> concat_lsb
          ; tlast = gnd
          ; tkeep = ones (512 / 8)
          ; tstrb = ones (512 / 8)
          }
      ; data_in_dest = { tready = load_sm.tready }
      ; done_ = cores.done_
      }
    ;;

    let hierarchy ~build_mode scope =
      let module Hier = Hierarchy.In_scope (I) (O) in
      Hier.hierarchical ~name:"kernel" ~scope (create ~build_mode)
    ;;
  end

  module Kernel_for_vitis = struct
    module Axi512 = Hardcaml_axi.Axi512

    module I = struct
      type 'a t =
        { ap_clk : 'a
        ; ap_rst_n : 'a
        ; controller_to_compute : 'a Axi512.Stream.Source.t [@rtlmangle true]
        ; compute_to_controller_dest : 'a Axi512.Stream.Dest.t
              [@rtlprefix "compute_to_controller_"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t =
        { compute_to_controller : 'a Axi512.Stream.Source.t [@rtlmangle true]
        ; controller_to_compute_dest : 'a Axi512.Stream.Dest.t
              [@rtlprefix "controller_to_compute_"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    let create
        ~build_mode
        scope
        { I.ap_clk = clock
        ; ap_rst_n = clear_n
        ; controller_to_compute
        ; compute_to_controller_dest
        }
      =
      let clear = ~:clear_n in
      let spec = Reg_spec.create ~clock ~clear () in
      let start = wire 1 in
      let first_4step_pass = reg_fb spec ~enable:start ~width:1 ~f:( ~: ) -- "4STEP" in
      let { Kernel.O.data_out; data_in_dest; done_ } =
        Kernel.hierarchy
          ~build_mode
          scope
          { clock
          ; clear
          ; first_4step_pass
          ; data_in = controller_to_compute
          ; data_out_dest = compute_to_controller_dest
          ; start
          }
      in
      start <== (done_ &: controller_to_compute.tvalid);
      { O.compute_to_controller = data_out; controller_to_compute_dest = data_in_dest }
    ;;
  end
end
