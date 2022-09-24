open Core
open Hardcaml
open Signal
module Axi512 = Hardcaml_axi.Axi512

module Make (Config : Config.S) = struct
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

  (* Host Streaming Assumptions:
   * - Points: Assume 64b alignment for coordinates, 512b alignment for points 
   * - Scalars: Assume 64b alignment for scalars, packed in whole numbers to 512b words 
   * (i.e. for 253-bit scalars, they are packed 2-per ddr word)
   *)
  let num_64b_words bits = Int.round_up bits ~to_multiple_of:64 / 64
  let num_scalar_64b_words = num_64b_words Config.scalar_bits
  let num_coord_64b_words = num_64b_words Config.field_bits
  let num_ddr_64b_words = 512 / 64

  let num_ddr_words num_64b_words =
    Int.round_up num_64b_words ~to_multiple_of:num_ddr_64b_words / num_ddr_64b_words
  ;;

  let num_point_ddr_words = num_ddr_words (3 * num_coord_64b_words)
  let num_scalars_per_ddr_word = num_ddr_64b_words / num_scalar_64b_words

  (* for simple implementation, assume that each scalar fits in 1 ddr word, and can be stacked on top of the final coordinate *)
  let () =
    assert (Config.scalar_bits <= 512);
    assert (
      num_ddr_words ((3 * num_coord_64b_words) + num_scalar_64b_words)
      = num_point_ddr_words)
  ;;

  module State = struct
    type t =
      | Point_words
      | Final_word
    [@@deriving sexp_of, compare, enumerate]
  end

  let create
    scope
    ({ clock; clear; host_scalars_to_fpga; ddr_points_to_fpga; host_to_fpga_dest } :
      _ I.t)
    : _ O.t
    =
    let ( -- ) = Scope.naming scope in
    let spec = Reg_spec.create ~clock ~clear () in
    let open Always in
    let host_to_fpga = Axi512.Stream.Source.Of_always.wire zero in
    let host_scalars_to_fpga_dest = Axi512.Stream.Dest.Of_always.wire zero in
    let ddr_points_to_fpga_dest = Axi512.Stream.Dest.Of_always.wire zero in
    let scalar_count =
      Variable.reg spec ~width:(Int.ceil_log2 num_scalars_per_ddr_word)
    in
    let point_word_count =
      Variable.reg spec ~width:(Int.ceil_log2 (num_point_ddr_words - 1))
    in
    let sm = State_machine.create (module State) spec in
    ignore (sm.current -- "state" : Signal.t);
    compile
      [ sm.switch
          [ ( Point_words
            , [ (* combinationally pass through the points stream *)
                Axi512.Stream.Source.Of_always.assign
                  host_to_fpga
                  { ddr_points_to_fpga with tlast = gnd }
              ; Axi512.Stream.Dest.Of_always.assign
                  ddr_points_to_fpga_dest
                  host_to_fpga_dest
              ; when_
                  (host_to_fpga.tvalid.value &: host_to_fpga_dest.tready)
                  [ point_word_count <-- point_word_count.value +:. 1
                  ; when_
                      (point_word_count.value ==:. num_point_ddr_words - 1 - 1)
                      [ point_word_count <--. 0; sm.set_next Final_word ]
                  ]
              ] )
          ; ( Final_word
            , let num_point_64b_words_remaining =
                (3 * num_coord_64b_words) - ((num_point_ddr_words - 1) * num_ddr_64b_words)
              in
              (* need to combine the bottom 2 words of the point word with the scalar *)
              let tdata =
                let point_data =
                  sel_bottom ddr_points_to_fpga.tdata (64 * num_point_64b_words_remaining)
                in
                let scalar_data =
                  mux
                    scalar_count.value
                    (split_lsb
                       host_scalars_to_fpga.tdata
                       ~part_width:(64 * num_scalar_64b_words))
                in
                uresize (scalar_data @: point_data) 512
              in
              let tvalid = host_scalars_to_fpga.tvalid &: ddr_points_to_fpga.tvalid in
              (* it's fine to just take tlast from the point stream because they should be in lock-step *)
              let tlast = ddr_points_to_fpga.tlast in
              [ Axi512.Stream.Source.Of_always.(
                  assign
                    host_to_fpga
                    { tdata; tvalid; tkeep = ones 64; tstrb = ones 64; tlast })
              ; when_
                  (host_to_fpga.tvalid.value &: host_to_fpga_dest.tready)
                  [ ddr_points_to_fpga_dest.tready <--. 1
                  ; scalar_count <-- scalar_count.value +:. 1
                  ; when_
                      (tlast |: (scalar_count.value ==:. num_scalars_per_ddr_word - 1))
                      [ scalar_count <--. 0; host_scalars_to_fpga_dest.tready <--. 1 ]
                  ; sm.set_next Point_words
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
end
