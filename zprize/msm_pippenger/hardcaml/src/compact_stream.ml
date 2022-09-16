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

  let num_256_words =
    (Int.round_up ~to_multiple_of:256 (Config.field_bits * 3)
    + Int.round_up ~to_multiple_of:256 Config.scalar_bits)
    / 256
  ;;

  let create (_ : Scope.t) { I.clock; clear; up; dn_dest } =
    let spec = Reg_spec.create ~clock ~clear () in
    let cnt = Always.Variable.reg ~width:(Int.ceil_log2 num_256_words) spec in
    let cnt_is_last_word = cnt.value ==:. num_256_words - 1 in
    let incr_cnt =
      let open Always in
      let cnt_next = mux2 cnt_is_last_word (zero (width cnt.value)) (cnt.value +:. 1) in
      cnt <-- cnt_next
    in
    let dn_tdata = mux cnt.value.:(0) [ sel_bottom up.tdata 256; sel_top up.tdata 256 ] in
    let dn_tvalid = up.tvalid in
    let dn_tlast = cnt_is_last_word &: up.tlast in
    let up_tready = cnt_is_last_word &: dn_dest.tready in
    Always.(compile [ when_ (dn_dest.tready &: dn_tvalid) [ incr_cnt ] ]);
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
end
