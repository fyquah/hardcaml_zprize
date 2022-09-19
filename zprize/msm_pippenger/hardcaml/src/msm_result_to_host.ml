open Core
open Hardcaml
open Hardcaml_axi
open Signal

module Make (Config : Config.S) = struct
  module Mixed_add = Twisted_edwards_lib.Mixed_add.Make (struct
    let num_bits = Config.field_bits
  end)

  module Xyzt = Mixed_add.Xyzt

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; result_point : 'a Xyzt.t
      ; result_point_valid : 'a
      ; last_result_point : 'a
      ; fpga_to_host_dest : 'a Axi512.Stream.Dest.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { fpga_to_host : 'a Axi512.Stream.Source.t
      ; result_point_ready : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  let aligned_to = 64

  module Xyzt_aligned =
    Interface.Update
      (Xyzt)
      (struct
        let t =
          Xyzt.(
            map2 port_names port_widths ~f:(fun n w ->
              n, Int.round_up w ~to_multiple_of:aligned_to))
        ;;
      end)

  let axi_bitwidth = 512

  let xyzt_to_aligned p =
    List.map2_exn
      Xyzt_aligned.(port_widths |> to_list)
      (Xyzt.to_alist p)
      ~f:(fun w (n, s) -> n, uresize s w)
    |> Xyzt_aligned.of_alist
  ;;

  module Xyzt_aligned_with_valid = With_valid.Wrap.Make (Xyzt_aligned)

  let aligned_point_512_words =
    Int.round_up
      Xyzt_aligned.(fold port_widths ~init:0 ~f:( + ))
      ~to_multiple_of:axi_bitwidth
    / axi_bitwidth
  ;;

  let aligned_point_512_words_without_t =
    Int.round_up
      Xyzt_aligned.(fold port_widths ~init:0 ~f:( + ) - Xyzt_aligned.port_widths.t)
      ~to_multiple_of:axi_bitwidth
    / axi_bitwidth
  ;;

  let create ~drop_t _scope (i : _ I.t) : _ O.t =
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let open Always in
    let result_point_ready = Variable.wire ~default:gnd in
    let aligned_result_point = Xyzt_aligned_with_valid.Of_always.reg spec in
    let last_result_l = Variable.reg spec ~width:1 in
    let tlast = Variable.reg spec ~width:1 in
    let valid_words =
      Variable.reg spec ~width:(num_bits_to_represent aligned_point_512_words)
    in
    ignore (valid_words.value -- "valid_words" : Signal.t);
    compile
      [ result_point_ready <-- ~:(aligned_result_point.valid.value)
      ; (* Shift out aligned point when the host is ready. *)
        when_
          (i.fpga_to_host_dest.tready &: aligned_result_point.valid.value)
          [ Xyzt_aligned.(
              Of_always.assign
                aligned_result_point.value
                (srl
                   (Of_signal.pack (Of_always.value aligned_result_point.value))
                   axi_bitwidth
                |> Of_signal.unpack))
          ; valid_words <-- valid_words.value -:. 1
          ; when_ (valid_words.value ==:. 2) [ tlast <-- last_result_l.value ]
          ; when_
              (valid_words.value ==:. 1)
              [ aligned_result_point.valid <-- gnd
              ; tlast <-- gnd
              ; last_result_l <-- gnd
              ; result_point_ready <-- vdd
              ]
          ]
      ; (* Detect when we are able to load another point from the MSM top. *)
        when_
          (result_point_ready.value &: i.result_point_valid)
          [ Xyzt.Of_always.assign
              aligned_result_point.value
              (xyzt_to_aligned i.result_point)
          ; last_result_l <-- i.last_result_point
          ; if_
              drop_t
              [ aligned_result_point.value.t <--. 0
              ; valid_words <--. aligned_point_512_words_without_t
              ]
              [ valid_words <--. aligned_point_512_words ]
          ; aligned_result_point.valid <-- vdd
          ]
      ];
    { O.result_point_ready = result_point_ready.value
    ; fpga_to_host =
        { tdata =
            sel_bottom
              Xyzt_aligned.(Of_signal.pack (Of_always.value aligned_result_point.value))
              axi_bitwidth
        ; tstrb = ones (axi_bitwidth / 8)
        ; tkeep = ones (axi_bitwidth / 8)
        ; tlast = tlast.value
        ; tvalid = aligned_result_point.valid.value
        }
    }
  ;;

  let hierarchical ?instance ~drop_t scope =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ?instance ~name:"msm_result_to_host" ~scope (create ~drop_t)
  ;;
end
