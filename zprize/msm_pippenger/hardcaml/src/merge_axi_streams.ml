open Hardcaml
open Signal
module Axi512 = Hardcaml_axi.Axi512

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; host_scalars_to_fpga : 'a Axi512.Stream.Source.t
         [@rtlprefix "host_scalars_to_fpga$src$"]
    ; ddr_points_to_fpga : 'a Axi512.Stream.Source.t
         [@rtlprefix "ddr_points_to_fpga$src$"]
    ; host_to_fpga_dest : 'a Axi512.Stream.Dest.t [@rtlprefix "host_to_fpga$dst$"]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { host_scalars_to_fpga_dest : 'a Axi512.Stream.Dest.t
         [@rtlprefix "host_scalars_to_fpga$dst$"]
    ; ddr_points_to_fpga_dest : 'a Axi512.Stream.Dest.t
         [@rtlprefix "ddr_points_to_fpga$dst$"]
    ; host_to_fpga : 'a Axi512.Stream.Source.t [@rtlprefix "host_to_fpga$src$"]
    }
  [@@deriving sexp_of, hardcaml]
end

(* for now, assume 512b alignment for scalars and points *)
let num_scalar_64b_words = 4 (* 2 per 512b word *)
let num_coord_64b_words = 6 (* 3 per 3 512b words - padding at end *)
let num_ddr_64b_words = 512 / 64

module State = struct
  type t =
    | Word_1
    | Word_2
    | Word_3
  [@@deriving sexp_of, compare, enumerate]
end

let create
  scope
  ({ clock; clear; host_scalars_to_fpga; ddr_points_to_fpga; host_to_fpga_dest } : _ I.t)
  : _ O.t
  =
  let ( -- ) = Scope.naming scope in
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  let host_to_fpga = Axi512.Stream.Source.Of_always.wire zero in
  let host_scalars_to_fpga_dest = Axi512.Stream.Dest.Of_always.wire zero in
  let ddr_points_to_fpga_dest = Axi512.Stream.Dest.Of_always.wire zero in
  let scalar_beat = Variable.reg spec ~width:1 in
  let sm = State_machine.create (module State) spec in
  ignore (sm.current -- "state" : Signal.t);
  let combinationally_pass_through_points next_state =
    [ (* combinationally pass through the points stream *)
      Axi512.Stream.Source.Of_always.assign
        host_to_fpga
        { ddr_points_to_fpga with tlast = gnd }
    ; Axi512.Stream.Dest.Of_always.assign ddr_points_to_fpga_dest host_to_fpga_dest
    ; when_
        (host_to_fpga.tvalid.value &: host_to_fpga_dest.tready)
        [ sm.set_next next_state ]
    ]
  in
  compile
    [ sm.switch
        [ Word_1, combinationally_pass_through_points Word_2
        ; Word_2, combinationally_pass_through_points Word_3
        ; ( Word_3
          , let num_point_64b_words_remaining =
              (3 * num_coord_64b_words) - (2 * num_ddr_64b_words)
            in
            assert (num_point_64b_words_remaining = 2);
            (* need to combine the bottom 2 words of the point word with the scalar *)
            let tdata =
              let point_data =
                sel_bottom ddr_points_to_fpga.tdata (64 * num_point_64b_words_remaining)
              in
              let scalar_data =
                assert (64 * num_scalar_64b_words == 256);
                mux2
                  (scalar_beat.value ==:. 1)
                  (drop_bottom host_scalars_to_fpga.tdata (64 * num_scalar_64b_words))
                  (sel_bottom host_scalars_to_fpga.tdata (64 * num_scalar_64b_words))
              in
              uresize (scalar_data @: point_data) 512
            in
            let tvalid = host_scalars_to_fpga.tvalid &: ddr_points_to_fpga.tvalid in
            (* it's fine to just take tlast from the point stream because they should be in lock-step *)
            let tlast = ddr_points_to_fpga.tvalid in
            [ Axi512.Stream.Source.Of_always.(
                assign
                  host_to_fpga
                  { tdata; tvalid; tkeep = ones 64; tstrb = ones 64; tlast })
            ; when_
                (host_to_fpga.tvalid.value &: host_to_fpga_dest.tready)
                [ ddr_points_to_fpga_dest.tready <--. 1
                ; when_
                    (scalar_beat.value ==:. 1)
                    [ host_scalars_to_fpga_dest.tready <--. 1 ]
                ; scalar_beat <-- ~:(scalar_beat.value)
                ; sm.set_next Word_1
                ]
            ] )
        ]
    ];
  { O.host_scalars_to_fpga_dest =
      Axi512.Stream.Dest.Of_always.value host_scalars_to_fpga_dest
  ; ddr_points_to_fpga_dest = Axi512.Stream.Dest.Of_always.value ddr_points_to_fpga_dest
  ; host_to_fpga = Axi512.Stream.Source.Of_always.value host_to_fpga
  }
;;

let hierarchical ?instance scope =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ?instance ~name:"merge_axi_streams" ~scope create
;;
