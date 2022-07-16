open Base
open Hardcaml
open Hardcaml_axi
open Signal
module Write_port_1d = Hardcaml_xilinx.Memory_builder.Write_port_1d

module Make (Config : Config.S) = struct
  let log_precomputed_points_table_size =
    Int.ceil_log2 Config.precomputed_points_table_size
  ;;

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; stream : 'a Axi256.Stream.Source.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Precomputed_point_wr =
    Write_port_1d.Specialize
      (Affine_point_or_infinity)
      (struct
        let address_width = log_precomputed_points_table_size
      end)

  module Encoding = struct
    type 'a t =
      { x : 'a [@bits 377]
      ; unused : 'a [@bits 7]
      ; y : 'a [@bits 377]
      ; is_infinity : 'a [@bits 1]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let decode_window (window : Signal.t) =
    let { Encoding.x; unused = _; y; is_infinity } = Encoding.Of_signal.unpack window in
    { Affine_point_or_infinity.point = { x; y }; is_infinity }
  ;;

  module O = struct
    type 'a t = { precomputed_points_wr : 'a Precomputed_point_wr.t }
    [@@deriving sexp_of, hardcaml]
  end

  module State = struct
    type t =
      | S_idle
      | S_streaming
    [@@deriving sexp_of, enumerate, compare]
  end

  let rotate (x : Always.Variable.t) =
    let open Always in
    x <-- lsbs x.value @: msb x.value
  ;;

  let create (scope : Scope.t) { I.clock; clear; stream } =
    let ( -- ) = Scope.naming scope in
    let spec = Reg_spec.create ~clock ~clear () in
    let address = Always.Variable.reg ~width:log_precomputed_points_table_size spec in
    let sm = Always.State_machine.create (module State) spec in
    let shift_reg = Always.Variable.reg ~width:3 spec in
    let window =
      let spec_no_clear = Reg_spec.create ~clock () in
      let tdata = stream.tdata in
      let tdata_reg = reg spec_no_clear ~enable:stream.tvalid tdata in
      let tdata_reg_reg = reg spec_no_clear ~enable:stream.tvalid tdata_reg in
      tdata @: tdata_reg @: tdata_reg_reg
    in
    Always.(
      compile
        [ sm.switch
            [ ( S_idle
              , [ when_
                    (stream.tvalid &: ~:(stream.tlast))
                    [ shift_reg <--. 1
                    ; address
                      <-- sel_bottom stream.tdata log_precomputed_points_table_size
                    ; sm.set_next S_streaming
                    ]
                ] )
            ; ( S_streaming
              , [ when_
                    stream.tvalid
                    [ rotate shift_reg
                    ; when_ stream.tlast [ shift_reg <--. 0; sm.set_next S_idle ]
                    ; when_ (msb shift_reg.value) [ address <-- address.value +:. 1 ]
                    ]
                ] )
            ]
        ]);
    ignore (sm.current -- "state" : Signal.t);
    ignore (shift_reg.value -- "shift_reg" : Signal.t);
    { O.precomputed_points_wr =
        { address = address.value
        ; enable = stream.tvalid &: msb shift_reg.value
        ; data = decode_window window
        }
    }
  ;;

  let hierarchical scope i =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~name:"decode_precomputed_points_stream" ~scope create i
  ;;
end
