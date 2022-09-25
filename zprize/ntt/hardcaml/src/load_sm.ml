open Base
open Hardcaml
open Signal

module Make (Config : Hardcaml_ntt.Core_config.S) = struct
  open Config

  let blocks = 1 lsl Config.logblocks
  let logsync = 1

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
      ; tvalid : 'a
      ; start : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { done_ : 'a
      ; tready : 'a
      ; wr_addr : 'a [@bits logn]
      ; wr_en : 'a [@bits blocks]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Var = Always.Variable

  let create _scope (i : _ I.t) =
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = Always.State_machine.create (module State) spec in
    let addr = Var.reg spec ~width:(logn + logblocks) in
    let addr_next = addr.value +:. 1 in
    let sync = Var.reg spec ~width:logsync in
    let tvalid = i.tvalid in
    Always.(
      compile
        [ sm.switch
            [ Start, [ addr <--. 0; when_ i.start [ sm.set_next Stream ] ]
            ; ( Stream
              , [ when_
                    tvalid
                    [ addr <-- addr_next; when_ (addr_next ==:. 0) [ sm.set_next Sync ] ]
                ] )
            ; ( Sync
              , [ sync <-- sync.value +:. 1
                ; when_ (sync.value ==:. -1) [ sm.set_next Start ]
                ] )
            ]
        ]);
    let block =
      if logblocks = 0
      then gnd
      else
        (* mux2 *)
        (*   i.first_4step_pass *)
        (*   (sel_bottom addr.value logblocks) *)
        drop_bottom addr.value logn
    in
    let addr =
      (* mux2 *)
      (*   i.first_4step_pass *)
      (*   (drop_bottom addr.value logblocks) *)
      sel_bottom addr.value logn
    in
    let block1h = binary_to_onehot block in
    let mask_by_block x =
      if Config.logblocks = 0 then x else repeat x blocks &: block1h
    in
    let done_ = sm.is Start in
    let processing = sm.is Stream in
    { O.done_
    ; tready = processing
    ; wr_en = mask_by_block (processing &: tvalid)
    ; wr_addr = addr
    }
  ;;

  let hierarchy scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~name:"load_sm" ~scope create
  ;;
end
