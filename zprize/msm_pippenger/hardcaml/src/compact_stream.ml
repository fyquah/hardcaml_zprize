open Core
open Hardcaml
open Hardcaml_axi
open Signal

module Make (Config : Config.S) = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; up : 'a Axi512.Stream.Source.t [@rtlprefix "up$"]
      ; dn_dest : 'a Axi256.Stream.Dest.t [@rtlprefix "dn$"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { dn : 'a Axi256.Stream.Source.t [@rtlprefix "dn$"]
      ; up_dest : 'a Axi512.Stream.Dest.t [@rtlprefix "up$"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let num_256_words_per_point =
    (Int.round_up ~to_multiple_of:256 (Config.field_bits * 3)
    + Int.round_up ~to_multiple_of:256 Config.scalar_bits)
    / 256
  ;;

  (* This is not caught halving it from 512 to 256. Suppose we only need 3*256
   * bits, we will only emit x0[255:0], x0[511:256] and x1[255:0]
   *)

  let create (_ : Scope.t) { I.clock; clear; up; dn_dest } =
    let spec = Reg_spec.create ~clock ~clear () in
    let cnt_for_point_reg =
      Always.Variable.reg ~width:(Int.ceil_log2 num_256_words_per_point) spec
    in
    let up_tready_mask_reg = Always.Variable.reg ~width:1 spec in
    let cnt_for_point_is_last_reg = Always.Variable.reg ~width:1 spec in
    let cnt_for_point_with_incr =
      mux2
        cnt_for_point_is_last_reg.value
        (zero (width cnt_for_point_reg.value))
        (cnt_for_point_reg.value +:. 1)
    in
    let dn_tdata =
      mux cnt_for_point_reg.value.:(0) [ sel_bottom up.tdata 256; sel_top up.tdata 256 ]
    in
    let dn_tvalid = up.tvalid in
    let dn_tlast = cnt_for_point_is_last_reg.value &: up.tlast in
    let up_tready = dn_dest.tready &: up_tready_mask_reg.value in
    let cnt_for_point_is_last_next_when_incr =
      cnt_for_point_with_incr ==:. num_256_words_per_point - 1
    in
    Always.(
      compile
        [ when_
            (dn_dest.tready &: dn_tvalid)
            [ cnt_for_point_reg <-- cnt_for_point_with_incr
            ; cnt_for_point_is_last_reg <-- cnt_for_point_is_last_next_when_incr
            ; up_tready_mask_reg
              <-- (cnt_for_point_with_incr.:(0) |: cnt_for_point_is_last_next_when_incr)
            ]
        ]);
    { O.dn =
        { tdata = dn_tdata
        ; tvalid = dn_tvalid
        ; tlast = dn_tlast
        ; tkeep = ones 32
        ; tstrb = ones 32
        }
    ; up_dest = { tready = up_tready }
    }
  ;;

  let hierarchical scope =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~name:"compact_stream" ~scope create
  ;;
end
