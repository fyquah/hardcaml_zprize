open Base
open Hardcaml
open Signal

module Make (Config : Hardcaml_ntt.Core_config.S) = struct
  open Config

  let blocks = 1 lsl Config.logblocks

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
      ; block : 'a [@bits max 1 Config.logblocks]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Var = Always.Variable

  let create (i : _ I.t) =
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = Always.State_machine.create (module State) spec in
    let addr = Var.reg spec ~width:(logn + 1) in
    let addr_next = addr.value +:. 1 in
    let block = Var.reg spec ~width:(max 1 logblocks) in
    let rd_en = Var.wire ~default:gnd in
    let tvalid = Var.reg spec ~width:1 in
    let tready = i.tready in
    Always.(
      compile
        [ sm.switch
            [ Start, [ block <--. 0; addr <--. 0; when_ i.start [ sm.set_next Preroll ] ]
            ; Preroll, [ rd_en <-- vdd; addr <--. 1; tvalid <-- vdd; sm.set_next Stream ]
            ; ( Stream
              , [ when_
                    tready
                    [ addr <-- addr_next
                    ; rd_en <-- vdd
                    ; when_
                        (addr.value ==:. 1 lsl logn)
                        [ tvalid <-- gnd
                        ; addr <--. 0
                        ; block <-- block.value +:. 1
                        ; (if Config.logblocks = 0
                          then sm.set_next Start
                          else
                            if_
                              (block.value ==:. blocks - 1)
                              [ sm.set_next Start ]
                              [ sm.set_next Preroll ])
                        ]
                    ]
                ] )
            ]
        ]);
    let block1h = binary_to_onehot block.value in
    let mask_by_block x =
      if Config.logblocks = 0 then x else repeat x blocks &: block1h
    in
    let done_ = sm.is Start in
    { O.done_
    ; tvalid = tvalid.value
    ; rd_addr = addr.value.:[logn - 1, 0]
    ; rd_en = mask_by_block rd_en.value
    ; block = block.value
    }
  ;;
end
