(** Special module to create a D FF hierarchically that can be pblocked easily.  *)

open Core
open Hardcaml
open Signal

let named_register ?clear ~scope ~clock ~slr x =
  match slr with
  | None ->
    let clear = Option.value clear ~default:gnd in
    Signal.reg (Reg_spec.create ~clock ~clear ()) x
  | Some slr ->
    (match clear with
     | None ->
       let module I = struct
         type 'a t =
           { clock : 'a
           ; d : 'a [@bits width x]
           }
         [@@deriving sexp_of, hardcaml]
       end
       in
       let module O = struct
         type 'a t = { q : 'a [@bits width x] } [@@deriving sexp_of, hardcaml]
       end
       in
       let module H = Hierarchy.In_scope (I) (O) in
       let create _ { I.clock; d } = { O.q = Signal.reg (Reg_spec.create ~clock ()) d } in
       let instance = Printf.sprintf "named_register_SLR%d" slr in
       let { O.q } =
         H.hierarchical
           ~name:"named_register"
           ~instance
           ~attributes:[ Rtl_attribute.Vivado.keep_hierarchy true ]
           ~scope
           create
           { I.clock; d = x }
       in
       q
     | Some clear ->
       let module I = struct
         type 'a t =
           { clock : 'a
           ; clear : 'a
           ; d : 'a [@bits width x]
           }
         [@@deriving sexp_of, hardcaml]
       end
       in
       let module O = struct
         type 'a t = { q : 'a [@bits width x] } [@@deriving sexp_of, hardcaml]
       end
       in
       let module H = Hierarchy.In_scope (I) (O) in
       let create _ { I.clock; clear; d } =
         { O.q = Signal.reg (Reg_spec.create ~clock ~clear ()) d }
       in
       let instance = Printf.sprintf "named_register_SLR%d" slr in
       let { O.q } =
         H.hierarchical
           ~name:"named_register"
           ~instance
           ~attributes:[ Rtl_attribute.Vivado.keep_hierarchy true ]
           ~scope
           create
           { I.clock; clear; d = x }
       in
       q)
;;
