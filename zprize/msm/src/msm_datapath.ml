open Core
open Hardcaml
open Hardcaml_axi
open Hardcaml_xilinx
open Signal

include struct
  open Elliptic_curve_lib
  module Ec_fpn_dbl = Ec_fpn_dbl
end

module Write_port_1d = Hardcaml_xilinx.Memory_builder.Write_port_1d

module State = struct
  type t =
    | S_idle
    | S_work_on_bit_position
    | S_wait_for_bit_position_finish
    | S_handle_doubling_fault
    | S_stream_result
  [@@deriving compare, enumerate, sexp_of]
end

module Make (Config : Config.S) = struct
  open Config
  module Stage_interfaces = Stage_interfaces.Make (Config)

  module Intermediate_point_lookup_stage =
    Intermediate_point_lookup_stage.Make (Config) (Stage_interfaces)

  module Doubling_stage = Doubling_stage.Make (Config) (Stage_interfaces)

  module Precomputed_point_lookup_stage =
    Precomputed_point_lookup_stage.Make (Config) (Stage_interfaces)

  module Adding_stage = Adding_stage.Make (Config) (Stage_interfaces)

  module Intermediate_point_writeback_stage =
    Intermediate_point_writeback_stage.Make (Config) (Stage_interfaces)

  module Precomputed_point_wr =
    Memory_builder.Write_port_1d.Specialize
      (Affine_point_or_infinity)
      (struct
        let address_width = Int.ceil_log2 Config.precomputed_points_table_size
      end)

  let log_datapath_num_entries = Int.ceil_log2 datapath_num_entries

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; precomputed_points_addr : 'a Axi16.Stream.Source.t
      ; precomputed_points_wr : 'a Precomputed_point_wr.t
      ; num_entries_minus_1 : 'a [@bits log_datapath_num_entries]
      ; scalar_num_bits_minus_1 : 'a [@bits 9]
      ; result_ready : 'a
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:true]
  end

  module O = struct
    type 'a t =
      { precomputed_points_addr_dest : 'a Axi16.Stream.Dest.t
      ; result : 'a Jacobian_point_or_infinity.With_valid.t
      ; result_last : 'a
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:true]
  end

  let create_intermediate_points_memory ~instance ~build_mode ~scope ~clock =
    let module M = Memory_builder.Create (Jacobian_point_with_metadata) in
    let depth = datapath_num_entries in
    M.create_simple_1d
      ~name:"intermediate_points_memory"
      ~build_mode
      ~instance
      ~depth
      ~ram_read_latency:intermediate_points_table_read_latency
      ~how_to_instantiate_ram:(Xpm Ultraram)
      ~scope
      ~clock
      ~clear:gnd
      ()
  ;;

  let create_precomputed_points_memory
      ~build_mode
      ~scope
      ~clock
      ~clear
      ~precomputed_points_wr
    =
    let module M = Memory_builder.Create (Affine_point_or_infinity) in
    let depth = precomputed_points_table_size in
    let memory_builder =
      M.create_simple_1d
        ~build_mode
        ~instance:"precomputed_points_memory"
        ~depth
        ~ram_read_latency:precomputed_points_table_read_latency
        ~how_to_instantiate_ram:(Xpm Ultraram)
        ~scope
        ~clock
        ~clear
        ()
    in
    Memory_builder.set_write_port_1d memory_builder B precomputed_points_wr;
    memory_builder
  ;;

  type datapath_output =
    { intermediate_point_lookup_stage :
        Signal.t Stage_interfaces.Intermediate_point_lookup.Stage_output.t
    ; intermediate_point_writeback_stage :
        Signal.t Stage_interfaces.Intermediate_point_writeback_stage.Stage_output.t
    }

  let instantiate_datapath
      ~clock
      ~scope
      ~intermediate_points_memory
      ~precomputed_points_memory
      datapath_input
    =
    let intermediate_point_lookup_stage =
      Intermediate_point_lookup_stage.create
        ~scope
        ~clock
        ~intermediate_points_memory
        datapath_input
    in
    let point_doubling_stage =
      Doubling_stage.create ~clock ~scope intermediate_point_lookup_stage
    in
    let precomputed_point_lookup_stage =
      Precomputed_point_lookup_stage.create
        ~scope
        ~clock
        ~precomputed_points_memory
        point_doubling_stage
    in
    let point_adding_stage =
      Adding_stage.create ~clock ~scope precomputed_point_lookup_stage
    in
    let intermediate_point_writeback_stage =
      Intermediate_point_writeback_stage.create
        ~clock
        ~intermediate_points_memory
        ~point_adding_stage
        ~point_doubling_stage
    in
    { intermediate_point_lookup_stage; intermediate_point_writeback_stage }
  ;;

  let incr_var (v : Always.Variable.t) = Always.(v <-- v.value +:. 1)

  let create
      ~build_mode
      scope
      ({ I.clock
       ; clear
       ; precomputed_points_wr
       ; precomputed_points_addr
       ; num_entries_minus_1
       ; scalar_num_bits_minus_1
       ; result_ready
       } :
        _ I.t)
    =
    let ( -- ) = Scope.naming scope in
    let spec_with_clear = Reg_spec.create ~clock ~clear () in
    let precomputed_points_memory =
      create_precomputed_points_memory
        ~build_mode
        ~scope
        ~clock
        ~clear
        ~precomputed_points_wr
    in
    let sm = Always.State_machine.create (module State) spec_with_clear in
    let intermediate_points_memory =
      create_intermediate_points_memory
        ~clock
        ~instance:"intermediate_points_memory"
        ~build_mode
        ~scope
    in
    let entry_index =
      Always.Variable.reg ~width:log_datapath_num_entries spec_with_clear
    in
    ignore (entry_index.value -- "entry_index" : Signal.t);
    let is_first = Always.Variable.reg ~width:1 spec_with_clear in
    let is_seen_doubling_fault = Always.Variable.reg ~width:1 spec_with_clear in
    let precomputed_points_addr_ready = Always.Variable.reg ~width:1 spec_with_clear in
    let bit_position_index = Always.Variable.reg ~width:9 spec_with_clear in
    let entry_index_is_last = entry_index.value ==: num_entries_minus_1 in
    let datapath_valid_wire = Always.Variable.wire ~default:gnd in
    let { intermediate_point_writeback_stage; intermediate_point_lookup_stage } =
      instantiate_datapath
        ~clock
        ~scope:(Scope.sub_scope scope "datapath")
        ~intermediate_points_memory
        ~precomputed_points_memory
        { entry_index = entry_index.value
        ; precomputed_point_address =
            sel_bottom
              precomputed_points_addr.tdata
              (Int.ceil_log2 precomputed_points_table_size)
        ; valid = datapath_valid_wire.value
        ; is_first = is_first.value
        ; is_handling_doubling_fault = sm.is S_handle_doubling_fault
        ; is_streaming_result = sm.is S_stream_result
        ; is_last = entry_index_is_last
        }
    in
    let datapath_output_doubling_fault =
      intermediate_point_writeback_stage.intermediate_point.requires_extra_doubling
      &: intermediate_point_writeback_stage.valid
    in
    let bit_position_index_is_last =
      bit_position_index.value ==: scalar_num_bits_minus_1
    in
    let result_fifo_empty = wire 1 in
    let result_fifo_nearly_full = wire 1 in
    let goto_stream_results =
      Always.(
        proc
          [ precomputed_points_addr_ready <--. 0
          ; entry_index <--. 0
          ; sm.set_next S_stream_result
          ])
    in
    let goto_work_on_next_bit_position =
      Always.(
        proc
          [ incr_var bit_position_index
          ; entry_index <--. 0
          ; precomputed_points_addr_ready <--. 1
          ; sm.set_next S_work_on_bit_position
          ])
    in
    let is_seen_doubling_fault_now_or_previously =
      is_seen_doubling_fault.value |: datapath_output_doubling_fault
    in
    let beat = Always.Variable.reg ~width:1 spec_with_clear in
    Always.(
      compile
        [ sm.switch
            [ ( S_idle
              , [ when_
                    precomputed_points_addr.tvalid
                    [ entry_index <--. 0
                    ; bit_position_index <--. 0
                    ; is_seen_doubling_fault <--. 0
                    ; precomputed_points_addr_ready <--. 1
                    ; is_first <--. 1
                    ; sm.set_next S_work_on_bit_position
                    ]
                ] )
            ; ( S_work_on_bit_position
              , [ precomputed_points_addr_ready <--. 1
                ; is_seen_doubling_fault <-- is_seen_doubling_fault_now_or_previously
                ; datapath_valid_wire
                  <-- (precomputed_points_addr_ready.value
                      &: precomputed_points_addr.tvalid)
                ; when_
                    (precomputed_points_addr.tvalid &: precomputed_points_addr_ready.value)
                    [ incr_var entry_index
                    ; precomputed_points_addr_ready <--. 0
                    ; when_
                        entry_index_is_last
                        [ is_first <--. 0; sm.set_next S_wait_for_bit_position_finish ]
                    ]
                ] )
            ; ( S_wait_for_bit_position_finish
              , [ is_seen_doubling_fault <-- is_seen_doubling_fault_now_or_previously
                ; when_
                    (intermediate_point_writeback_stage.valid
                    &: (intermediate_point_writeback_stage.entry_index
                       ==: num_entries_minus_1))
                    [ is_seen_doubling_fault <--. 0
                    ; if_
                        is_seen_doubling_fault_now_or_previously
                        [ entry_index <--. 0
                        ; beat <--. 1
                        ; sm.set_next S_handle_doubling_fault
                        ]
                      @@ [ (* In this branch, there is no doubling fault, we can
                            * just move on to the next bit position.*)
                           if_ bit_position_index_is_last [ goto_stream_results ]
                           @@ [ goto_work_on_next_bit_position ]
                         ]
                    ]
                ] )
            ; ( S_handle_doubling_fault
              , [ datapath_valid_wire <-- beat.value
                ; beat <-- ~:(beat.value)
                ; when_
                    beat.value
                    [ incr_var entry_index
                    ; when_
                        entry_index_is_last
                        [ beat <--. 0; sm.set_next S_wait_for_bit_position_finish ]
                    ]
                ] )
            ; ( S_stream_result
              , [ datapath_valid_wire <-- ~:result_fifo_nearly_full
                ; when_
                    ~:result_fifo_nearly_full
                    [ incr_var entry_index
                    ; when_
                        (entry_index.value ==: num_entries_minus_1)
                        [ sm.set_next S_idle ]
                    ]
                ] )
            ]
        ]);
    let result_fifo =
      let d =
        intermediate_point_lookup_stage.is_last
        @: Jacobian_point_or_infinity.Of_signal.pack
             intermediate_point_lookup_stage.intermediate_point.point
      in
      Fifo.create
        ~scope:(Scope.sub_scope scope "result_fifo")
        ~clock
        ~clear
        ~showahead:true
        ~capacity:(2 * Config.intermediate_points_table_read_latency)
        ~nearly_full:Config.intermediate_points_table_read_latency
        ~wr:
          (intermediate_point_lookup_stage.valid
          &: intermediate_point_lookup_stage.is_streaming_result)
        ~d
        ~rd:(result_ready &: ~:result_fifo_empty)
        ()
    in
    ignore (sm.current -- "state" : Signal.t);
    result_fifo_nearly_full <== result_fifo.nearly_full;
    result_fifo_empty <== (result_fifo.empty &: ~:clear);
    Memory_builder.complete intermediate_points_memory;
    Memory_builder.complete precomputed_points_memory;
    { O.precomputed_points_addr_dest = { tready = precomputed_points_addr_ready.value }
    ; result =
        { With_valid.valid = ~:result_fifo_empty
        ; value = Jacobian_point_or_infinity.Of_signal.unpack (lsbs result_fifo.q)
        }
    ; result_last = msb result_fifo.q
    }
  ;;

  let hierarchical ~build_mode scope i =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"msm_datapath" (create ~build_mode) i
  ;;
end
