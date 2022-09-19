open Base
open Hardcaml
open Signal

module Make (Config : Hardcaml_ntt.Core_config.S) = struct
  open Config

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
      ; rd_en : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Var = Always.Variable

  let create (i : _ I.t) =
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = Always.State_machine.create (module State) spec in
    let addr = Var.reg spec ~width:(logn + 1) in
    let addr_next = addr.value +:. 1 in
    let rd_en = Var.wire ~default:gnd in
    let tvalid = Var.reg spec ~width:1 in
    Always.(
      compile
        [ sm.switch
            [ Start, [ addr <--. 0; when_ i.start [ sm.set_next Preroll ] ]
            ; Preroll, [ rd_en <-- vdd; addr <--. 1; tvalid <-- vdd; sm.set_next Stream ]
            ; ( Stream
              , [ when_
                    i.tready
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
    { O.done_
    ; tvalid = tvalid.value
    ; rd_addr = addr.value.:[logn - 1, 0]
    ; rd_en = rd_en.value
    }
  ;;
end
