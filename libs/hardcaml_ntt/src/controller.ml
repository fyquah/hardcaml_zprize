open! Base
open! Hardcaml

module Make (Config : Core_config.S) = struct
  module Var = Always.Variable

  let logn = Config.logn
  let support_4step_twiddle = Config.support_4step_twiddle

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; first_iter : 'a
      ; first_4step_pass : 'a
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
      ; omegas : 'a list
           [@bits Gf.Signal.num_bits] [@length Twiddle_factor_stream.pipe_length]
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

  let create scope (inputs : _ I.t) =
    let open Signal in
    let ( -- ) = Scope.naming scope in
    let spec = Reg_spec.create ~clock:inputs.clock ~clear:inputs.clear () in
    let sm = Always.State_machine.create (module State) spec in
    ignore (sm.current -- "STATE" : Signal.t);
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
      List.init logn ~f:(fun i -> Twiddle_factor_stream.initial_pipeline_factors (i + 1))
      |> List.transpose_exn
      |> List.map ~f:(mux i.value)
      |> Array.of_list
    in
    let start_twiddles = Var.reg spec ~width:1 in
    let first_stage = Var.reg spec ~width:1 in
    let last_stage = Var.reg spec ~width:1 in
    let twiddle_stage = Var.reg spec ~width:1 in
    let twiddle_update = Var.reg spec ~width:1 in
    let sync_count = Var.reg spec ~width:Twiddle_update.sync_cycles_width in
    let sync_count_next = sync_count.value +:. 1 in
    let flip = Var.wire ~default:gnd in
    let read_write_enable = Var.wire ~default:gnd in
    let if_twiddle_supported ((s : State.t), p) =
      if support_4step_twiddle then s, p else s, []
    in
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
                    (sync_count.value ==:. Twiddle_update.sync_cycles - 1)
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
                      (sync_count.value ==:. Twiddle_update.sync_cycles - 1)
                      [ twiddle_stage <--. 0
                      ; twiddle_update <--. 0
                      ; last_stage <-- gnd
                      ; done_ <--. 1
                      ; sm.set_next Idle
                      ]
                  ] )
            ]
        ]);
    { O.done_ = done_.value
    ; i = i.value
    ; j = j.value
    ; k = k.value
    ; m = m.value
    ; addr1 = addr1.value
    ; addr2 = addr2.value
    ; omegas = List.init Twiddle_factor_stream.pipe_length ~f:(fun idx -> omegas.(idx))
    ; start_twiddles = start_twiddles.value
    ; first_stage = first_stage.value
    ; last_stage = last_stage.value
    ; twiddle_stage = twiddle_stage.value
    ; twiddle_update = { valid = twiddle_update.value; index = sync_count.value }
    ; read_write_enable = read_write_enable.value
    ; flip = flip.value
    }
  ;;

  let hierarchy scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~name:"ctrl" ~scope create
  ;;
end
