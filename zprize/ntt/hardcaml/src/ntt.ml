open! Base
open! Hardcaml

type twiddle_4step_config =
  { rows_per_iteration : int
  ; log_num_iterations : int
  }
[@@deriving sexp_of]

module type Config = sig
  val logn : int
  val twiddle_4step_config : twiddle_4step_config option
end

module Gf = Gf_bits.Make (Hardcaml.Signal)

module Make (Config : Config) = struct
  let logn = Config.logn
  let n = 1 lsl logn
  let multiply_latency = 3
  let ram_output_pipelining = 1
  let ram_latency = 1
  let datapath_latency = ram_latency + ram_output_pipelining + multiply_latency
  let support_4step_twiddle = Option.is_some Config.twiddle_4step_config

  let twiddle_4step_config =
    Option.value
      ~default:{ rows_per_iteration = 0; log_num_iterations = 0 }
      Config.twiddle_4step_config
  ;;

  let gf_mul ~clock a b =
    let pipe x =
      Gf.to_bits x |> Signal.pipeline (Reg_spec.create ~clock ()) ~n:1 |> Gf.of_bits
    in
    Gf.mul ~pipe (Gf.of_bits a) (Gf.of_bits b) |> Gf.to_bits
  ;;

  module Twiddle_factor_stream = struct
    let pipe_length = multiply_latency + 1

    module I = struct
      type 'a t =
        { clock : 'a
        ; start_twiddles : 'a
        ; omegas : 'a list [@bits Gf.num_bits] [@length pipe_length]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t = { w : 'a [@bits Gf.num_bits] } [@@deriving sexp_of, hardcaml]
    end

    module Var = Always.Variable
    open Signal

    let create _scope (i : _ I.t) =
      let pipe_length = List.length i.omegas in
      let spec = Reg_spec.create ~clock:i.clock () in
      let w = Always.Variable.reg ~width:Gf.num_bits spec in
      let omega_step, omega_pipe = List.last_exn i.omegas, List.drop_last_exn i.omegas in
      let multiplier_output = gf_mul ~clock:i.clock w.value omega_step in
      let count = Var.reg spec ~width:(Int.ceil_log2 (multiply_latency + 1)) in
      Always.(
        compile
          [ w <-- mux count.value (omega_pipe @ [ multiplier_output ])
          ; when_ (count.value <>:. pipe_length - 1) [ count <-- count.value +:. 1 ]
          ; when_ i.start_twiddles [ w <-- Gf.to_bits Gf.one; count <--. 0 ]
          ]);
      { O.w = w.value }
    ;;

    let hierarchy scope =
      let module Hier = Hierarchy.In_scope (I) (O) in
      Hier.hierarchical ~name:"twdl" ~scope create
    ;;

    let initial_pipeline_factors root =
      let inverse_root = Roots.inverse.(root) in
      let rec f acc i =
        if i = pipe_length then [] else acc :: f (Gf_z.( * ) acc inverse_root) (i + 1)
      in
      f inverse_root 0 |> List.map ~f:(fun x -> Gf.to_bits (Gf.of_z (Gf_z.to_z x)))
    ;;
  end

  module Controller = struct
    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; start : 'a
        ; first_iter : 'a
        ; first_4step_pass : 'a
        ; twiddle_update_in : 'a [@bits Gf.num_bits]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module Twiddle_update = struct
      type 'a t =
        { valid : 'a
        ; factors : 'a array [@bits Gf.num_bits] [@length 2]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t =
        { done_ : 'a
        ; i : 'a [@bits Int.ceil_log2 (logn + 1)]
        ; j : 'a [@bits logn]
        ; k : 'a [@bits logn]
        ; m : 'a [@bits logn]
        ; addr1 : 'a [@bits logn]
        ; addr2 : 'a [@bits logn]
        ; omegas : 'a list [@bits Gf.num_bits] [@length Twiddle_factor_stream.pipe_length]
        ; start_twiddles : 'a
        ; first_stage : 'a
        ; last_stage : 'a
        ; twiddle_stage : 'a
        ; twiddle_update : 'a Twiddle_update.t
        ; read_write_enable : 'a
        ; flip : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module State = struct
      type t =
        | Idle
        | Looping
        | Sync
        | Twiddle
        | Sync_twiddles
      [@@deriving compare, enumerate, sexp_of, variants]
    end

    module Var = Always.Variable

    let gf_z_to_bits g = Gf_z.to_z g |> Gf.of_z |> Gf.to_bits
    let twiddle_root row iter = Gf_z.pow Roots.inverse.(logn * 2) (row * (iter + 1))

    let twiddle_scale_z =
      List.init Twiddle_factor_stream.pipe_length ~f:(fun col ->
        twiddle_root twiddle_4step_config.rows_per_iteration col)
    ;;

    let twiddle_scale = twiddle_scale_z |> List.map ~f:gf_z_to_bits
    let for_ lo hi f = List.range lo (hi + 1) |> List.map ~f |> Always.proc

    let create ?(row = 0) scope (inputs : _ I.t) =
      let open Signal in
      let ( -- ) = Scope.naming scope in
      let spec = Reg_spec.create ~clock:inputs.clock ~clear:inputs.clear () in
      let sm = Always.State_machine.create (module State) spec in
      ignore (sm.current -- "STATE" : Signal.t);
      let block =
        Option.map Config.twiddle_4step_config ~f:(fun { log_num_iterations; _ } ->
          Var.reg spec ~width:log_num_iterations)
      in
      Option.iter block ~f:(fun block -> ignore (block.value -- "BLOCK"));
      let done_ = Var.reg (Reg_spec.override spec ~clear_to:vdd) ~width:1 in
      let i = Var.reg spec ~width:(Int.ceil_log2 (logn + 1)) in
      ignore (i.value -- "i" : Signal.t);
      let i_next = i.value +:. 1 in
      let j = Var.reg spec ~width:logn in
      ignore (j.value -- "j" : Signal.t);
      let j_next = j.value +:. 1 in
      let k = Var.reg spec ~width:logn in
      ignore (k.value -- "k" : Signal.t);
      let m = Var.reg spec ~width:logn in
      ignore (m.value -- "m" : Signal.t);
      let m_next = sll m.value 1 in
      let k_next = k.value +: m_next in
      let addr1 = Var.reg spec ~width:logn in
      let addr2 = Var.reg spec ~width:logn in
      let addr2_plus1 = addr2.value +:. 1 in
      let omegas =
        List.init logn ~f:(fun i ->
          Twiddle_factor_stream.initial_pipeline_factors (i + 1))
        |> List.transpose_exn
        |> List.map ~f:(mux i.value)
        |> Array.of_list
      in
      let start_twiddles = Var.reg spec ~width:1 in
      let first_stage = Var.reg spec ~width:1 in
      let last_stage = Var.reg spec ~width:1 in
      let twiddle_stage = Var.reg spec ~width:1 in
      let twiddle_update = Var.reg spec ~width:1 in
      let sync_cycles =
        max
          (Twiddle_factor_stream.pipe_length + ram_output_pipelining + 1)
          (datapath_latency + 1)
      in
      let sync_count = Var.reg spec ~width:(max 1 (Int.ceil_log2 sync_cycles)) in
      let sync_count_next = sync_count.value +:. 1 in
      let flip = Var.wire ~default:gnd in
      let read_write_enable = Var.wire ~default:gnd in
      let if_twiddle_supported ((s : State.t), p) =
        if support_4step_twiddle then s, p else s, []
      in
      let twiddle_omegas =
        Array.init Twiddle_factor_stream.pipe_length ~f:(fun col ->
          Var.reg
            (Reg_spec.override ~clear_to:(twiddle_root row col |> gf_z_to_bits) spec)
            ~width:Gf.num_bits)
      in
      Array.iteri twiddle_omegas ~f:(fun i t ->
        ignore (t.value -- ("twiddle_omega" ^ Int.to_string i) : Signal.t));
      Always.(
        compile
          [ start_twiddles <--. 0
          ; sm.switch
              [ ( Idle
                , [ when_
                      inputs.start
                      [ i <--. 0
                      ; j <--. 0
                      ; k <--. 0
                      ; m <--. 1
                      ; addr1 <--. 0
                      ; addr2 <--. 1
                      ; done_ <--. 0
                      ; start_twiddles <--. 1
                      ; first_stage <--. 1
                      ; last_stage <--. 0
                      ; twiddle_stage <--. 0
                      ; twiddle_update <--. 0
                      ; when_
                          inputs.first_iter
                          [ for_ 0 (Twiddle_factor_stream.pipe_length - 1) (fun col ->
                              twiddle_omegas.(col)
                              <-- (twiddle_root row col |> gf_z_to_bits))
                          ]
                      ; sm.set_next Looping
                      ]
                  ] )
              ; ( Looping
                , [ j <-- j_next
                  ; read_write_enable <-- vdd
                  ; addr1 <-- addr1.value +:. 1
                  ; addr2 <-- addr2_plus1
                  ; when_
                      (j_next ==: m.value)
                      [ if_
                          (k_next ==:. 0)
                          [ sm.set_next Sync ]
                          [ j <--. 0
                          ; start_twiddles <--. 1
                          ; k <-- k_next
                          ; addr1 <-- k_next
                          ; addr2 <-- k_next +: m.value
                          ]
                      ]
                  ] )
              ; ( Sync
                , [ sync_count <-- sync_count_next
                  ; when_
                      (sync_count.value ==:. sync_cycles - 1)
                      [ sm.set_next Looping
                      ; sync_count <--. 0
                      ; start_twiddles <--. 1
                      ; flip <-- vdd
                      ; first_stage <--. 0
                      ; i <-- i_next
                      ; j <--. 0
                      ; k <-- k_next
                      ; m <-- m_next
                      ; addr1 <-- k_next
                      ; addr2 <-- k_next +: m_next
                      ; when_
                          (i_next ==:. logn - 1)
                          [ last_stage <-- ~:(inputs.first_4step_pass) ]
                      ; when_
                          (i_next ==:. logn)
                          [ last_stage <--. 0
                          ; (if support_4step_twiddle
                            then
                              proc
                                [ if_
                                    inputs.first_4step_pass
                                    [ last_stage <--. 1
                                    ; twiddle_stage <--. 1
                                    ; addr2 <--. 0
                                    ; sm.set_next Twiddle
                                    ]
                                    [ done_ <--. 1; sm.set_next Idle ]
                                ]
                            else proc [ done_ <--. 1; sm.set_next Idle ])
                          ]
                      ]
                  ] )
              ; if_twiddle_supported
                  ( Twiddle
                  , [ addr2 <-- addr2_plus1
                    ; read_write_enable <-- vdd
                    ; when_
                        (addr2.value ==:. -1)
                        [ addr2 <--. 0
                        ; twiddle_update <--. 1
                        ; twiddle_stage <--. 0
                        ; sm.set_next Sync_twiddles
                        ]
                    ] )
              ; if_twiddle_supported
                  ( Sync_twiddles
                  , [ sync_count <-- sync_count_next
                    ; when_
                        (sync_count.value ==:. Twiddle_factor_stream.pipe_length - 1)
                        [ twiddle_update <--. 0 ]
                    ; when_
                        (sync_count.value ==:. sync_cycles - 1)
                        [ twiddle_stage <--. 0
                        ; twiddle_update <--. 0
                        ; last_stage <-- gnd
                        ; done_ <--. 1
                        ; (match block with
                           | None -> proc []
                           | Some block -> block <-- block.value +:. 1)
                        ; sm.set_next Idle
                        ]
                    ] )
              ]
          ; (* latch updated twiddle omegas *)
            proc
              [ when_
                  (pipeline
                     spec
                     ~n:(Twiddle_factor_stream.pipe_length + ram_output_pipelining)
                     twiddle_update.value)
                  [ (* depending on the pipe dpeth, this might not work.*)
                    twiddle_omegas.(3) <-- inputs.twiddle_update_in
                  ; twiddle_omegas.(2) <-- twiddle_omegas.(3).value
                  ; twiddle_omegas.(1) <-- twiddle_omegas.(2).value
                  ; twiddle_omegas.(0) <-- twiddle_omegas.(1).value
                  ]
              ]
          ]);
      { O.done_ = done_.value
      ; i = i.value
      ; j = j.value
      ; k = k.value
      ; m = m.value
      ; addr1 = addr1.value
      ; addr2 = addr2.value
      ; omegas =
          (if support_4step_twiddle
          then
            List.init Twiddle_factor_stream.pipe_length ~f:(fun idx ->
              mux2 twiddle_stage.value twiddle_omegas.(idx).value omegas.(idx))
          else List.init Twiddle_factor_stream.pipe_length ~f:(fun idx -> omegas.(idx)))
      ; start_twiddles = start_twiddles.value
      ; first_stage = first_stage.value
      ; last_stage = last_stage.value
      ; twiddle_stage = twiddle_stage.value
      ; twiddle_update =
          { valid = twiddle_update.value
          ; factors =
              (let sync_count_piped =
                 pipeline ~n:(1 + ram_output_pipelining) spec sync_count.value
               in
               [| mux
                    sync_count_piped
                    (Array.to_list twiddle_omegas |> List.map ~f:(fun v -> v.value))
                ; mux sync_count_piped twiddle_scale
               |])
          }
      ; read_write_enable = read_write_enable.value
      ; flip = flip.value
      }
    ;;

    let hierarchy ?row scope =
      let module Hier = Hierarchy.In_scope (I) (O) in
      Hier.hierarchical ~name:"ctrl" ~scope (create ?row)
    ;;
  end

  (* Butterly and twiddle factor calculation *)
  module Datapath = struct
    open Signal

    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; d1 : 'a [@bits Gf.num_bits]
        ; d2 : 'a [@bits Gf.num_bits]
        ; omegas : 'a list [@bits Gf.num_bits] [@length Twiddle_factor_stream.pipe_length]
        ; start_twiddles : 'a
        ; twiddle_stage : 'a
        ; twiddle_update : 'a Controller.Twiddle_update.t
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t =
        { q1 : 'a [@bits Gf.num_bits]
        ; q2 : 'a [@bits Gf.num_bits]
        ; twiddle_update_q : 'a [@bits Gf.num_bits]
        }
      [@@deriving sexp_of, hardcaml]
    end

    let ( +: ) a b = Gf.( + ) (Gf.of_bits a) (Gf.of_bits b) |> Gf.to_bits
    let ( -: ) a b = Gf.( - ) (Gf.of_bits a) (Gf.of_bits b) |> Gf.to_bits
    let ( *: ) = `Dont_use_me
    let `Dont_use_me = ( *: )

    let create scope (i : _ I.t) =
      let ( -- ) = Scope.naming scope in
      let spec_with_clear = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
      let spec_no_clear = Reg_spec.create ~clock:i.clock () in
      let pipe ?(clear = false) ~n d =
        pipeline ~n (if clear then spec_with_clear else spec_no_clear) d
      in
      (* The latency of the input data must be adjusted to match the latency of
         the twiddle factor calculation *)
      let { Twiddle_factor_stream.O.w } =
        Twiddle_factor_stream.hierarchy
          scope
          { clock = i.clock
          ; start_twiddles = pipe ~n:ram_output_pipelining ~clear:true i.start_twiddles
          ; omegas = i.omegas
          }
      in
      let w = w -- "twiddle_factor" in
      let t =
        let a =
          mux2
            (pipe ~n:(1 + ram_output_pipelining) i.twiddle_update.valid)
            i.twiddle_update.factors.(0)
            (pipe ~n:ram_output_pipelining i.d2)
        in
        let b =
          mux2
            (pipe ~n:(1 + ram_output_pipelining) i.twiddle_update.valid)
            i.twiddle_update.factors.(1)
            w
        in
        gf_mul ~clock:i.clock a b -- "T"
      in
      let d1 = pipe ~n:(multiply_latency + ram_output_pipelining) i.d1 in
      let piped_twiddle_stage =
        pipe ~n:(multiply_latency + ram_output_pipelining + 1) i.twiddle_stage
        -- "piped_twiddle_stage"
      in
      { O.q1 = reg spec_no_clear (d1 +: t)
      ; q2 = reg spec_no_clear (mux2 piped_twiddle_stage t (d1 -: t))
      ; twiddle_update_q = t
      }
    ;;

    let hierarchy scope =
      let module Hier = Hierarchy.In_scope (I) (O) in
      Hier.hierarchical ~name:"dp" ~scope create
    ;;
  end

  module Core = struct
    open! Signal

    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; start : 'a
        ; first_iter : 'a
        ; first_4step_pass : 'a
        ; d1 : 'a [@bits Gf.num_bits]
        ; d2 : 'a [@bits Gf.num_bits]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t =
        { q1 : 'a [@bits Gf.num_bits]
        ; q2 : 'a [@bits Gf.num_bits]
        ; addr1_in : 'a [@bits logn]
        ; addr2_in : 'a [@bits logn]
        ; read_enable_in : 'a
        ; addr1_out : 'a [@bits logn]
        ; addr2_out : 'a [@bits logn]
        ; write_enable_out : 'a
        ; first_stage : 'a
        ; last_stage : 'a
        ; twiddle_stage : 'a
        ; flip : 'a
        ; done_ : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    let create ?row scope (i : _ I.t) =
      let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
      let datapath = Datapath.O.Of_signal.wires () in
      let controller =
        Controller.hierarchy
          ?row
          scope
          { Controller.I.clock = i.clock
          ; clear = i.clear
          ; start = i.start
          ; first_iter = i.first_iter
          ; first_4step_pass = i.first_4step_pass
          ; twiddle_update_in = datapath.twiddle_update_q
          }
      in
      Datapath.O.Of_signal.assign
        datapath
        (Datapath.hierarchy
           scope
           { Datapath.I.clock = i.clock
           ; clear = i.clear
           ; d1 = i.d1
           ; d2 = i.d2
           ; omegas = controller.omegas
           ; start_twiddles = controller.start_twiddles
           ; twiddle_stage = controller.twiddle_stage
           ; twiddle_update = controller.twiddle_update
           });
      let pipe = pipeline spec ~n:(datapath_latency + 1) in
      { O.q1 = datapath.q1
      ; q2 = datapath.q2
      ; addr1_in = controller.addr1
      ; addr2_in = controller.addr2
      ; read_enable_in = controller.read_write_enable
      ; addr1_out = controller.addr1 |> pipe
      ; addr2_out = controller.addr2 |> pipe
      ; write_enable_out = controller.read_write_enable |> pipe
      ; first_stage = controller.first_stage
      ; last_stage = controller.last_stage
      ; twiddle_stage = controller.twiddle_stage
      ; flip = controller.flip
      ; done_ = controller.done_
      }
    ;;

    let hierarchy ?row scope =
      let module Hier = Hierarchy.In_scope (I) (O) in
      Hier.hierarchical ~name:"core" ~scope (create ?row)
    ;;
  end

  module With_rams = struct
    open! Signal

    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; start : 'a
        ; first_4step_pass : 'a
        ; first_iter : 'a
        ; flip : 'a
        ; wr_d : 'a [@bits Gf.num_bits]
        ; wr_en : 'a
        ; wr_addr : 'a [@bits logn]
        ; rd_en : 'a
        ; rd_addr : 'a [@bits logn]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t =
        { done_ : 'a
        ; rd_q : 'a [@bits Gf.num_bits]
        }
      [@@deriving sexp_of, hardcaml]
    end

    let input_ram scope build_mode (i : _ I.t) (core : _ Core.O.t) =
      Bram.create_dual
        (Scope.sub_scope scope "ram_in")
        ~build_mode
        ~size:n
        ~clock:i.clock
        ~clear:i.clear
        ~flip:i.flip
        ~write_port_a:{ address = i.wr_addr; data = i.wr_d; enable = i.wr_en }
        ~write_port_b:{ address = zero logn; data = zero Gf.num_bits; enable = gnd }
        ~read_port_a:{ address = reverse core.addr1_in; enable = core.read_enable_in }
        ~read_port_b:{ address = reverse core.addr2_in; enable = core.read_enable_in }
    ;;

    let transpose_ram scope build_mode (i : _ I.t) (core : _ Core.O.t) ~flip ~last_stage =
      let scope = Scope.sub_scope scope "ram_transp" in
      let ( -- ) = Scope.naming scope in
      let q0, q1 =
        Bram.create_dual
          scope
          ~build_mode
          ~size:n
          ~clock:i.clock
          ~clear:i.clear
          ~flip
          ~write_port_a:
            { address = core.addr1_out
            ; data = core.q1
            ; enable = core.write_enable_out &: ~:last_stage
            }
          ~write_port_b:
            { address = core.addr2_out
            ; data = core.q2
            ; enable = core.write_enable_out &: ~:last_stage
            }
          ~read_port_a:{ address = core.addr1_in; enable = core.read_enable_in }
          ~read_port_b:{ address = core.addr2_in; enable = core.read_enable_in }
      in
      q0 -- "q0", q1 -- "q1"
    ;;

    let output_ram
      scope
      build_mode
      (i : _ I.t)
      (core : _ Core.O.t)
      ~last_stage
      ~twiddle_stage
      =
      let q, _ =
        Bram.create_dual
          (Scope.sub_scope scope "ram_out")
          ~build_mode
          ~size:n
          ~clock:i.clock
          ~clear:i.clear
          ~flip:i.flip
          ~write_port_a:
            { address = core.addr1_out
            ; data = core.q1
            ; enable = core.write_enable_out &: (last_stage &: ~:twiddle_stage)
            }
          ~write_port_b:
            { address = core.addr2_out
            ; data = core.q2
            ; enable = core.write_enable_out &: last_stage
            }
          ~read_port_a:{ address = i.rd_addr; enable = i.rd_en }
          ~read_port_b:{ address = zero logn; enable = gnd }
      in
      q
    ;;

    let create ?row ~build_mode scope (i : _ I.t) =
      let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
      let core = Core.O.Of_signal.wires () in
      let pipe ~n d = pipeline spec ~n d in
      (* input and output rams *)
      let d_in_0, d_in_1 = input_ram scope build_mode i core in
      let d_out_0, d_out_1 =
        transpose_ram scope build_mode i core ~flip:core.flip ~last_stage:core.last_stage
      in
      (* core *)
      Core.O.iter2
        core
        (Core.hierarchy
           ?row
           scope
           (let first_stage = pipe ~n:1 core.first_stage in
            { clock = i.clock
            ; clear = i.clear
            ; start = i.start
            ; first_iter = i.first_iter
            ; first_4step_pass = i.first_4step_pass
            ; d1 = mux2 first_stage d_in_0 d_out_0
            ; d2 = mux2 first_stage d_in_1 d_out_1
            }))
        ~f:( <== );
      let d_out =
        output_ram
          scope
          build_mode
          i
          core
          ~last_stage:(pipe ~n:(datapath_latency + 1) core.last_stage)
          ~twiddle_stage:(pipe ~n:(datapath_latency + 1) core.twiddle_stage)
      in
      { O.done_ = core.done_; rd_q = d_out }
    ;;

    let hierarchy ?row ?instance ~build_mode scope =
      let module Hier = Hierarchy.In_scope (I) (O) in
      Hier.hierarchical ~name:"ntt_with_rams" ?instance ~scope (create ?row ~build_mode)
    ;;
  end
end
