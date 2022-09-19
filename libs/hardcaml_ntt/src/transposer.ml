open Base
open Hardcaml
open Signal
module Axi512 = Hardcaml_axi.Axi512

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; transposer_in : 'a Axi512.Stream.Source.t [@rtlprefix "in_"]
    ; transposer_out_dest : 'a Axi512.Stream.Dest.t [@rtlprefix "out_"]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { transposer_out : 'a Axi512.Stream.Source.t [@rtlprefix "out_"]
    ; transposer_in_dest : 'a Axi512.Stream.Dest.t [@rtlprefix "in_"]
    }
  [@@deriving sexp_of, hardcaml]
end

let () = Hardcaml.Caller_id.set_mode Full_trace
let transposer_height = 512 / 64

module Make (M : sig
  val transposer_depth_in_cycles : int
end) =
struct
  include M
  module Var = Always.Variable

  let transposer_memory_depth = 2 * transposer_depth_in_cycles

  module Writer = struct
    module State = struct
      type t =
        | Wait_for_space_available
        | Writing
      [@@deriving sexp_of, compare, enumerate]
    end

    module O = struct
      type 'a t =
        { currently_writing : 'a [@bits 2]
        ; transposer_in_dest : 'a Axi512.Stream.Dest.t
        ; write_address : 'a [@bits Int.ceil_log2 transposer_memory_depth]
        ; write_enable : 'a [@bits transposer_height]
        ; write_data : 'a [@bits 512]
        }
      [@@deriving sexp_of, hardcaml]
    end
  end

  module Reader = struct
    module State = struct
      type t =
        | Wait_for_data_avaiable
        | Reading
      [@@deriving sexp_of, compare, enumerate]
    end

    module O = struct
      type 'a t =
        { currently_waiting_for : 'a [@bits 2]
        ; transposer_out : 'a Axi512.Stream.Source.t
        ; read_address : 'a [@bits Int.ceil_log2 transposer_memory_depth]
        }
      [@@deriving sexp_of, hardcaml]
    end
  end

  module Memories = struct
    module I = struct
      type 'a t =
        { clock : 'a
        ; read_address : 'a [@bits Int.ceil_log2 transposer_memory_depth]
        ; write_address : 'a [@bits Int.ceil_log2 transposer_memory_depth]
        ; write_enable : 'a [@bits transposer_height]
        ; write_data : 'a [@bits 512]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t = { read_data : 'a array [@length transposer_height] [@bits 512] }
      [@@deriving sexp_of, hardcaml]
    end

    let create { I.clock; read_address; write_address; write_enable; write_data } =
      let read_data =
        Array.init transposer_height ~f:(fun i ->
          Signal.multiport_memory
            transposer_memory_depth
            ~write_ports:
              [| { write_clock = clock
                 ; write_address
                 ; write_enable = write_enable.:(i)
                 ; write_data
                 }
              |]
            ~read_addresses:[| read_address |]
          |> Fn.flip Array.get 0
          |> Fn.flip Signal.add_attribute Rtl_attribute.Vivado.Ram_style.distributed)
      in
      { O.read_data }
    ;;

    let hierarchical scope i =
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical ~scope ~name:"transposer_memories" (fun _scope i -> create i) i
    ;;
  end

  let create_reader
    ~scope
    ~spec
    ~(writer : _ Writer.O.t)
    ~(transposer_out_dest : 'a Axi512.Stream.Dest.t)
    ~transposer_depth_in_cycles
    ~read_data
    =
    let ( -- ) = Scope.naming scope in
    let currently_waiting_for = Var.reg ~width:2 spec in
    let data_is_available =
      let currently_writing = writer.currently_writing in
      let currently_waiting_for = currently_waiting_for.value in
      currently_waiting_for
      ==: currently_writing -:. 1
      |: (currently_waiting_for ==: currently_writing -:. 2)
    in
    let sm = Always.State_machine.create (module Reader.State) spec in
    let sub_address =
      Var.reg ~width:(Int.max 1 (Int.ceil_log2 transposer_depth_in_cycles)) spec
    in
    let element_offset = Var.reg ~width:(Int.ceil_log2 8) spec in
    let tvalid = Var.reg ~width:1 spec in
    ignore (element_offset.value -- "element_offset" : Signal.t);
    Always.(
      compile
        [ sm.switch
            [ ( Wait_for_data_avaiable
              , [ sub_address <--. 0
                ; when_ data_is_available [ tvalid <--. 1; sm.set_next Reading ]
                ] )
            ; ( Reading
              , [ when_
                    transposer_out_dest.tready
                    [ element_offset <-- element_offset.value +:. 1
                    ; when_
                        (element_offset.value ==:. 8 - 1)
                        [ sub_address <-- sub_address.value +:. 1
                        ; when_
                            (sub_address.value ==:. transposer_depth_in_cycles - 1)
                            [ sub_address <--. 0
                            ; currently_waiting_for <-- currently_waiting_for.value +:. 1
                            ; tvalid <--. 0
                            ; sm.set_next Wait_for_data_avaiable
                            ]
                        ]
                    ]
                ] )
            ]
        ]);
    let read_address =
      match transposer_depth_in_cycles with
      | 1 -> currently_waiting_for.value.:(0)
      | _ -> currently_waiting_for.value.:(0) @: sub_address.value
    in
    let tdata =
      Array.map read_data ~f:(fun read_data ->
        read_data |> split_lsb ~part_width:64 |> mux element_offset.value)
      |> Array.to_list
      |> Signal.concat_lsb
    in
    (* CR fquah: Register this output? *)
    { Reader.O.currently_waiting_for = currently_waiting_for.value
    ; read_address
    ; transposer_out =
        { tvalid = tvalid.value; tdata; tkeep = ones 64; tstrb = ones 64; tlast = gnd }
    }
  ;;

  let create_writer
    ~spec
    ~(reader : _ Reader.O.t)
    ~transposer_depth_in_cycles
    ~(transposer_in : 'a Axi512.Stream.Source.t)
    : _ Writer.O.t
    =
    let currently_writing = Var.reg ~width:2 spec in
    let transposer_in_dest = Axi512.Stream.Dest.Of_always.reg spec in
    let sm = Always.State_machine.create (module Writer.State) spec in
    let can_start_writing =
      reader.currently_waiting_for
      ==: currently_writing.value
      |: (reader.currently_waiting_for ==: currently_writing.value -:. 1)
    in
    let sub_address =
      Var.reg ~width:(Int.max 1 (Int.ceil_log2 transposer_depth_in_cycles)) spec
    in
    let write_enables_mask = Var.reg ~width:transposer_height spec in
    let write_address =
      match transposer_depth_in_cycles with
      | 1 -> currently_writing.value.:(0)
      | _ -> currently_writing.value.:(0) @: sub_address.value
    in
    let write_enable =
      write_enables_mask.value
      &: repeat (sm.is Writing &: transposer_in.tvalid) transposer_height
    in
    Always.(
      compile
        [ sm.switch
            [ ( Wait_for_space_available
              , [ write_enables_mask <--. 0
                ; when_
                    can_start_writing
                    [ write_enables_mask <--. 1
                    ; transposer_in_dest.tready <--. 1
                    ; sm.set_next Writing
                    ]
                ] )
            ; ( Writing
              , [ when_
                    transposer_in.tvalid
                    [ sub_address <-- sub_address.value +:. 1
                    ; when_
                        (sub_address.value ==:. transposer_depth_in_cycles - 1)
                        [ write_enables_mask <-- sll write_enables_mask.value 1
                        ; sub_address <--. 0
                        ; when_
                            (msb write_enables_mask.value)
                            [ write_enables_mask <--. 0
                            ; transposer_in_dest.tready <--. 0
                            ; currently_writing <-- currently_writing.value +:. 1
                            ; sm.set_next Wait_for_space_available
                            ]
                        ]
                    ]
                ] )
            ]
        ]);
    { Writer.O.currently_writing = currently_writing.value
    ; transposer_in_dest = Axi512.Stream.Dest.Of_always.value transposer_in_dest
    ; write_address
    ; write_enable
    ; write_data = transposer_in.tdata
    }
  ;;

  let create (scope : Scope.t) { I.clock; clear; transposer_in; transposer_out_dest } =
    let ( -- ) = Scope.naming scope in
    let writer = Writer.O.Of_signal.wires () in
    let reader = Reader.O.Of_signal.wires () in
    let memories =
      Memories.hierarchical
        scope
        { clock
        ; read_address = reader.read_address
        ; write_address = writer.write_address
        ; write_enable = writer.write_enable
        ; write_data = writer.write_data
        }
    in
    let spec = Reg_spec.create ~clock ~clear () in
    Writer.O.Of_signal.assign
      writer
      (create_writer ~spec ~reader ~transposer_in ~transposer_depth_in_cycles);
    Reader.O.Of_signal.assign
      reader
      (create_reader
         ~scope:(Scope.sub_scope scope "reader")
         ~spec
         ~writer
         ~transposer_out_dest
         ~transposer_depth_in_cycles
         ~read_data:memories.read_data);
    ignore (writer.currently_writing -- "wr_pos");
    ignore (reader.currently_waiting_for -- "rd_pos");
    { O.transposer_out = reader.transposer_out
    ; transposer_in_dest = writer.transposer_in_dest
    }
  ;;
end

let create ~transposer_depth_in_cycles scope (i : _ I.t) =
  let open Make (struct
    let transposer_depth_in_cycles = transposer_depth_in_cycles
  end) in
  create scope i
;;

let hierarchical ~transposer_depth_in_cycles scope (i : _ I.t) =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~name:"transposer" ~scope (create ~transposer_depth_in_cycles) i
;;
