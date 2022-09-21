open Base
open Hardcaml
open Signal

module Make (Config : Hardcaml_ntt.Core_config.S) = struct
  open Config

  let blocks = 1 lsl Config.logblocks

  module State = struct
    type t =
      | Start
      | Stream
    [@@deriving sexp_of, compare, enumerate]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
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

  let create (i : _ I.t) =
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = Always.State_machine.create (module State) spec in
    let addr = Var.reg spec ~width:logn in
    let addr_next = addr.value +:. 1 in
    let block = Var.reg spec ~width:(max 1 logblocks) in
    let tvalid = i.tvalid in
    Always.(
      compile
        [ sm.switch
            [ Start, [ addr <--. 0; when_ i.start [ sm.set_next Stream ] ]
            ; ( Stream
              , [ when_
                    tvalid
                    [ addr <-- addr_next
                    ; when_
                        (addr_next ==:. 0)
                        [ block <-- block.value +:. 1
                        ; (if Config.logblocks = 0
                          then sm.set_next Start
                          else when_ (block.value ==:. blocks - 1) [ sm.set_next Start ])
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
    let processing = ~:done_ in
    { O.done_
    ; tready = processing
    ; wr_en = mask_by_block (processing &: tvalid)
    ; wr_addr = addr.value
    }
  ;;
end
