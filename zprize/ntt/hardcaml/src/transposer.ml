open Base
open Hardcaml
open Hardcaml_xilinx
open Signal
module Axi512 = Hardcaml_axi.Axi512

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; transposer_in : 'a Axi512.Stream.Source.t [@rtlmangle true]
    ; transposer_out_dest : 'a Axi512.Stream.Dest.t [@rtlprefix "transposer_out_"]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { transposer_out : 'a Axi512.Stream.Source.t [@rtlmangle true]
    ; transposer_in_dest : 'a Axi512.Stream.Dest.t [@rtlprefix "transposer_in_"]
    }
  [@@deriving sexp_of, hardcaml]
end

module Data = Hardcaml.Interface.Value (struct
  let port_width = 512
  let port_name = "data"
end)

let transposer_height = 512 / 64
let transposer_memory_depth ~transposer_depth_in_cycles = 2 * transposer_depth_in_cycles

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
      }
    [@@deriving sexp_of, hardcaml]
  end
end

let create_reader
  ~memories
  ~spec
  ~(writer : _ Writer.O.t)
  ~(transposer_out_dest : 'a Axi512.Stream.Dest.t)
  ~transposer_depth_in_cycles
  =
  let currently_waiting_for = Always.Variable.reg ~width:2 spec in
  let data_is_available =
    let currently_writing = writer.currently_writing in
    let currently_waiting_for = currently_waiting_for.value in
    currently_waiting_for
    ==: currently_writing -:. 1
    |: (currently_waiting_for ==: currently_writing -:. 2)
  in
  let sm = Always.State_machine.create (module Reader.State) spec in
  let sub_address =
    Always.Variable.reg ~width:(Int.ceil_log2 transposer_depth_in_cycles) spec
  in
  let element_offset =
    Always.Variable.reg ~width:(Int.ceil_log2 transposer_height) spec
  in
  let tvalid = Always.Variable.reg ~width:1 spec in
  Always.(
    compile
      [ sm.switch
          [ ( Wait_for_data_avaiable
            , [ sub_address <--. 0; when_ data_is_available [ sm.set_next Reading ] ] )
          ; ( Reading
            , [ when_
                  transposer_out_dest.tready
                  [ element_offset <-- element_offset.value +:. 1
                  ; when_
                      (element_offset.value ==:. transposer_height - 1)
                      [ sub_address <-- sub_address.value +:. 1
                      ; when_
                          (sub_address.value ==:. transposer_depth_in_cycles - 1)
                          [ sub_address <--. 0
                          ; currently_waiting_for <-- currently_waiting_for.value +:. 1
                          ; sm.set_next Wait_for_data_avaiable
                          ]
                      ]
                  ]
              ] )
          ]
      ]);
  let tdata =
    let address = currently_waiting_for.value.:(0) @: sub_address.value in
    Array.map memories ~f:(fun memory ->
      Memory_builder.set_read_port_1d memory B { address; enable = vdd }
      |> split_lsb ~part_width:64
      |> mux element_offset.value)
    |> Array.to_list
    |> Signal.concat_lsb
  in
  (* CR fquah: Register this output? *)
  { Reader.O.currently_waiting_for = currently_waiting_for.value
  ; transposer_out =
      { tvalid = tvalid.value; tdata; tkeep = ones 64; tstrb = ones 64; tlast = gnd }
  }
;;

let create_writer
  ~memories
  ~spec
  ~(reader : _ Reader.O.t)
  ~transposer_depth_in_cycles
  ~(transposer_in : 'a Axi512.Stream.Source.t)
  : _ Writer.O.t
  =
  let currently_writing = Always.Variable.reg ~width:2 spec in
  let transposer_in_dest = Axi512.Stream.Dest.Of_always.reg spec in
  let sm = Always.State_machine.create (module Writer.State) spec in
  let can_start_writing =
    reader.currently_waiting_for
    ==: currently_writing.value
    |: (reader.currently_waiting_for ==: currently_writing.value -:. 1)
  in
  let sub_address =
    Always.Variable.reg ~width:(Int.ceil_log2 transposer_depth_in_cycles) spec
  in
  let write_enables_mask = Always.Variable.reg ~width:(Array.length memories) spec in
  let address = currently_writing.value.:(0) @: sub_address.value in
  Array.iteri memories ~f:(fun i memory ->
    Memory_builder.set_write_port_1d
      memory
      B
      { address
      ; enable = write_enables_mask.value.:(i) &: sm.is Writing &: transposer_in.tvalid
      ; data = transposer_in.tdata
      });
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
  }
;;

let create
  ~build_mode
  ~transposer_depth_in_cycles
  (scope : Scope.t)
  { I.clock; clear; transposer_in; transposer_out_dest }
  =
  let memories =
    let open Memory_builder.Create (Data) in
    Array.init transposer_height ~f:(fun i ->
      (* 0 latency is allowed for LUTRams *)
      create_simple_1d
        ~instance:(Printf.sprintf "lut_mem_%d" i)
        ~build_mode
        ~depth:(transposer_memory_depth ~transposer_depth_in_cycles)
        ~ram_read_latency:0
        ~how_to_instantiate_ram:(Xpm Distributed)
        ~scope
        ~clock
        ~clear:gnd
        ())
  in
  let spec = Reg_spec.create ~clock ~clear () in
  let writer = Writer.O.Of_signal.wires () in
  let reader = Reader.O.Of_signal.wires () in
  Writer.O.Of_signal.assign
    writer
    (create_writer ~memories ~spec ~reader ~transposer_in ~transposer_depth_in_cycles);
  Reader.O.Of_signal.assign
    reader
    (create_reader
       ~memories
       ~spec
       ~writer
       ~transposer_out_dest
       ~transposer_depth_in_cycles);
  { O.transposer_out = reader.transposer_out
  ; transposer_in_dest = writer.transposer_in_dest
  }
;;
