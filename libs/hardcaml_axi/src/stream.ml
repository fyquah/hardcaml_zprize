open Base
open Hardcaml
open Stream_intf
open Signal

module type S = S

module Make (X : Config.S) = struct
  open X

  let tkeep_bits = data_bits / 8
  let tstrb_bits = data_bits / 8

  module Source = struct
    type 'a t =
      { tvalid : 'a
      ; tdata : 'a [@bits data_bits]
      ; tkeep : 'a [@bits tkeep_bits]
      ; tstrb : 'a [@bits tstrb_bits]
      ; tlast : 'a
      }
    [@@deriving sexp_of, hardcaml]

    let assign_all_but_tvalid { tvalid = _; tdata; tkeep; tstrb; tlast } src =
      let open Always in
      proc
        [ tdata <-- src.tdata
        ; tkeep <-- src.tkeep
        ; tstrb <-- src.tstrb
        ; tlast <-- src.tlast
        ]
    ;;
  end

  module Dest = struct
    type 'a t = { tready : 'a } [@@deriving sexp_of, hardcaml]
  end

  (* This is a port of the skid buffer as per
   * https://github.com/alexforencich/verilog-axis/blob/master/rtl/axis_register.v#L91
   *)
  module Register = struct
    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; up : 'a Source.t [@rtlprefix "up$"]
        ; dn_dest : 'a Dest.t [@rtlprefix "dn$"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t =
        { dn : 'a Source.t [@rtlprefix "dn$"]
        ; up_dest : 'a Dest.t [@rtlprefix "up$"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    let create _scope { I.clock; clear; up; dn_dest } =
      let spec = Reg_spec.create ~clock ~clear () in
      let s_axis = up in
      let m_axis_dest = dn_dest in
      let temp_m_axis_tvalid_reg = Always.Variable.reg ~width:1 spec in
      let m_axis_reg = Source.Of_always.reg spec in
      let s_axis_tready_reg = Always.Variable.reg ~width:1 spec in
      let temp_m_axis_reg = Source.Of_always.reg spec in
      let m_axis_tvalid_next = Always.Variable.wire ~default:m_axis_reg.tvalid.value in
      let temp_m_axis_tvalid_next =
        Always.Variable.wire ~default:temp_m_axis_tvalid_reg.value
      in
      let store_axis_input_to_output = Always.Variable.wire ~default:gnd in
      let store_axis_input_to_temp = Always.Variable.wire ~default:gnd in
      let store_axis_temp_to_output = Always.Variable.wire ~default:gnd in
      let s_axis_tready_early =
        m_axis_dest.tready
        |: (~:(temp_m_axis_tvalid_reg.value) &: ~:(m_axis_reg.tvalid.value))
      in
      (* Combinational updates. *)
      Always.(
        compile
          [ if_
              s_axis_tready_reg.value
              [ if_
                  (m_axis_dest.tready |: ~:(m_axis_reg.tvalid.value))
                  [ m_axis_tvalid_next <-- s_axis.tvalid
                  ; store_axis_input_to_output <--. 1
                  ]
                @@ [ temp_m_axis_tvalid_next <-- s_axis.tvalid
                   ; store_axis_input_to_temp <--. 1
                   ]
              ]
            @@ elif
                 m_axis_dest.tready
                 [ m_axis_tvalid_next <-- temp_m_axis_tvalid_reg.value
                 ; temp_m_axis_tvalid_next <--. 0
                 ; store_axis_temp_to_output <--. 1
                 ]
            @@ [ (* Do nothing *) ]
          ]);
      (* Register updates. *)
      Always.(
        compile
          [ s_axis_tready_reg <-- s_axis_tready_early
          ; m_axis_reg.tvalid <-- m_axis_tvalid_next.value
          ; temp_m_axis_tvalid_reg <-- temp_m_axis_tvalid_next.value
          ; if_
              store_axis_input_to_output.value
              [ Source.assign_all_but_tvalid m_axis_reg s_axis ]
            @@ elif
                 store_axis_temp_to_output.value
                 [ Source.assign_all_but_tvalid
                     m_axis_reg
                     (Source.Of_always.value temp_m_axis_reg)
                 ]
            @@ [ (* Do nothing *) ]
          ; when_
              store_axis_input_to_temp.value
              [ Source.Of_always.assign temp_m_axis_reg s_axis ]
          ]);
      { O.dn = Source.Of_always.value m_axis_reg
      ; up_dest = { tready = s_axis_tready_reg.value }
      }
    ;;

    let hierarchical scope i =
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical ~name:"axis_register" ~scope create i
    ;;
  end
end
