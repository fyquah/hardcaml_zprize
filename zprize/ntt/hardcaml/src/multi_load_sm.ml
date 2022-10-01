open Base
open Hardcaml
open Signal

module Make (Config : Top_config.S) = struct
  open Config

  let blocks = 1 lsl Config.logblocks
  let write_pipelining = 2
  let num_streams = 1 lsl log_num_streams

  module State = struct
    type t =
      | Start
      | Stream
      | Sync
    [@@deriving sexp_of, compare, enumerate]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; first_4step_pass : 'a
      ; tvalid : 'a [@bits num_streams]
      ; start : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { done_ : 'a
      ; tready : 'a [@bits num_streams]
      ; wr_addr : 'a [@bits logn]
      ; wr_en : 'a [@bits blocks]
      ; select : 'a [@bits log_num_streams]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Var = Always.Variable

  let arbitrate data =
    Hardcaml_circuits.Arbiters.Round_robin_with_priority.combinational
      (module Signal)
      ~index:(Offset (zero log_num_streams))
      ~data
  ;;

  let create scope (i : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = Always.State_machine.create (module State) spec in
    let addrs =
      Array.init num_streams ~f:(fun _ ->
        Var.reg spec ~width:(logn + logblocks - log_num_streams))
    in
    let sync = Var.reg spec ~width:(Int.ceil_log2 write_pipelining) in
    let active = Var.reg spec ~width:num_streams in
    ignore (active.value -- "active" : Signal.t);
    let active_next = Var.wire ~default:active.value in
    let { With_valid.value = select; valid = tvalid } =
      arbitrate
        (List.init num_streams ~f:(fun idx ->
           { With_valid.value = of_int ~width:log_num_streams idx
           ; valid = i.tvalid.:(idx) &: active.value.:(idx)
           }))
    in
    let incr_addr =
      Always.(
        proc
          (List.init num_streams ~f:(fun idx ->
             when_ i.tvalid.:(idx) [ addrs.(idx) <-- addrs.(idx).value +:. 1 ])))
    in
    let clear_addrs =
      Always.(proc (List.init num_streams ~f:(fun idx -> addrs.(idx) <--. 0)))
    in
    let addr = mux select (Array.to_list addrs |> List.map ~f:(fun x -> x.value)) in
    let deactivate =
      Always.(
        proc
          (List.init num_streams ~f:(fun idx ->
             when_
               i.tvalid.:(idx)
               [ active_next
                 <-- (active.value &: ~:(of_int ~width:num_streams (1 lsl idx)))
               ])))
    in
    Always.(
      compile
        [ deactivate
        ; sm.switch
            [ ( Start
              , [ clear_addrs
                ; sync <--. 0
                ; active <--. -1
                ; when_ i.start [ sm.set_next Stream ]
                ] )
            ; ( Stream
              , [ when_
                    tvalid
                    [ incr_addr
                    ; when_
                        (addr ==:. -1)
                        [ active <-- active_next.value
                        ; when_ (active_next.value ==:. 0) [ sm.set_next Sync ]
                        ]
                    ]
                ] )
            ; ( Sync
              , [ sync <-- sync.value +:. 1
                ; when_ (sync.value ==:. write_pipelining - 1) [ sm.set_next Start ]
                ] )
            ]
        ]);
    { O.done_ = sm.is Start
    ; tready =
        i.tvalid &: repeat (sm.is Stream) num_streams &: active.value
        (* XX aray: Probably cant do this...but maybe with a datapath register *)
    ; wr_addr = select @: addr
    ; wr_en = tvalid
    ; select
    }
  ;;

  let hierarchy scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~name:"load_sm" ~scope create
  ;;
end
