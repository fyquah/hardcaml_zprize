open Base
open Hardcaml
open Signal

module Make (Config : Hardcaml_ntt.Ntt.Config) = struct
  open Config

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
      ; wr_en : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Var = Always.Variable

  let create (i : _ I.t) =
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = Always.State_machine.create (module State) spec in
    let addr = Var.reg spec ~width:logn in
    let addr_next = addr.value +:. 1 in
    Always.(
      compile
        [ sm.switch
            [ Start, [ addr <--. 0; when_ i.start [ sm.set_next Stream ] ]
            ; ( Stream
              , [ when_
                    i.tvalid
                    [ addr <-- addr_next; when_ (addr_next ==:. 0) [ sm.set_next Start ] ]
                ] )
            ]
        ]);
    let done_ = sm.is Start in
    let processing = ~:done_ in
    { O.done_; tready = processing; wr_en = processing &: i.tvalid; wr_addr = addr.value }
  ;;
end
