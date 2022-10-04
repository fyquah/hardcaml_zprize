open Base
open Hardcaml
open Signal

module Make (Config : Top_config.S) = struct
  open Config

  let blocks = 1 lsl Config.logblocks
  let read_address_pipelining = 3
  let read_data_pipelining = 3
  let read_data_tree_mux_stages = 2

  let sync_cycles =
    Hardcaml_ntt.Core_config.ram_latency
    + read_data_pipelining
    + read_address_pipelining
    + read_data_tree_mux_stages
  ;;

  module State = struct
    type t =
      | Start
      | Preroll
      | Stream
    [@@deriving sexp_of, compare, enumerate]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; first_4step_pass : 'a
      ; tready : 'a
      ; start : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { done_ : 'a
      ; tvalid : 'a
      ; rd_addr : 'a [@bits logn]
      ; rd_en : 'a [@bits blocks]
      ; rd_any : 'a
      ; block : 'a [@bits max 1 Config.logblocks]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Var = Always.Variable

  let create _scope (i : _ I.t) =
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = Always.State_machine.create (module State) spec in
    let addr = Var.reg spec ~width:(logn + logblocks + 1) in
    let addr_next = addr.value +:. 1 in
    let sync = Var.reg spec ~width:(Int.ceil_log2 sync_cycles) in
    let rd_en = Var.wire ~default:gnd in
    let tvalid = Var.reg spec ~width:1 in
    let tready = i.tready in
    Always.(
      compile
        [ sm.switch
            [ Start, [ addr <--. 0; sync <--. 0; when_ i.start [ sm.set_next Preroll ] ]
            ; ( Preroll
              , [ rd_en <-- vdd
                ; addr <-- addr_next
                ; sync <-- sync.value +:. 1
                ; when_
                    (sync.value ==:. sync_cycles - 1)
                    [ tvalid <-- vdd; sm.set_next Stream ]
                ] )
            ; ( Stream
              , [ when_
                    tready
                    [ addr <-- addr_next
                    ; rd_en <-- vdd
                    ; when_
                        (addr.value ==:. (1 lsl (logn + logblocks)) + (sync_cycles - 1))
                        [ tvalid <-- gnd; addr <--. 0; sm.set_next Start ]
                    ]
                ] )
            ]
        ]);
    let addr = lsbs addr.value in
    let block =
      if logblocks = 0
      then gnd
      else (
        match memory_layout with
        | Normal_layout_single_port | Optimised_layout_single_port ->
          mux2
            i.first_4step_pass
            (sel_bottom (drop_bottom addr logcores) logblocks)
            (sel_bottom addr logblocks)
        | Normal_layout_multi_port -> raise_s [%message "not implemented"])
    in
    let addr =
      match memory_layout with
      | Normal_layout_single_port | Optimised_layout_single_port ->
        mux2
          i.first_4step_pass
          (if logcores = 0
          then drop_bottom addr logblocks
          else drop_bottom addr (logblocks + logcores) @: sel_bottom addr logcores)
          (drop_bottom addr logblocks)
      | Normal_layout_multi_port -> raise_s [%message "not implemented"]
    in
    let block1h = binary_to_onehot block in
    let mask_by_block x =
      if Config.logblocks = 0 then x else repeat x blocks &: block1h
    in
    let done_ = sm.is Start in
    { O.done_
    ; tvalid = tvalid.value
    ; rd_addr = addr
    ; rd_en = mask_by_block rd_en.value
    ; rd_any = rd_en.value
    ; block
    }
  ;;

  let hierarchy scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~name:"store_sm" ~scope create
  ;;
end
