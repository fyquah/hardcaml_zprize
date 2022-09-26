open Base
open Hardcaml
open Signal

module Make (Config : Config.S) = struct
  open Config

  let log_num_windows = Int.ceil_log2 num_windows
  let stall_fifo_depth = 1 lsl log_stall_fifo_depth

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; push : 'a
      ; scalar : 'a [@bits window_size_bits] [@rtlprefix "i_"]
      ; negative : 'a [@rtlprefix "i_"]
      ; window : 'a [@bits log_num_windows]
      ; affine_point : 'a [@bits affine_point_bits]
      ; pop : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { all_windows_have_stall : 'a
      ; some_windows_are_full : 'a
      ; all_windows_are_empty : 'a
      ; current_window_has_stall : 'a
      ; affine_point_out : 'a [@bits affine_point_bits]
      ; scalar_out : 'a [@bits window_size_bits]
      ; scalar_out_valid : 'a
      ; negative_out : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O_window = struct
    type 'a t =
      { empty : 'a
      ; not_empty : 'a
      ; full : 'a
      ; read_address : 'a [@bits log_stall_fifo_depth]
      ; write_address : 'a [@bits log_stall_fifo_depth]
      ; scalar : 'a [@bits window_size_bits]
      ; negative : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Var = Always.Variable

  module Scalar = struct
    type 'a t =
      { scalar : 'a [@bits window_size_bits]
      ; negative : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create_window scope (i : _ I.t) ~window_index =
    let ( -- ) = Scope.naming scope in
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let enable = i.window ==:. window_index in
    let read_address = Var.reg spec ~width:log_stall_fifo_depth in
    let write_address = Var.reg spec ~width:log_stall_fifo_depth in
    let level = Var.reg spec ~width:(log_stall_fifo_depth + 1) in
    ignore (level.value -- "LEVEL" : Signal.t);
    let push = i.push &: enable in
    let pop = i.pop &: enable in
    let ram =
      memory
        stall_fifo_depth
        ~write_port:
          { write_clock = i.clock
          ; write_address = write_address.value
          ; write_enable = push
          ; write_data =
              Scalar.Of_signal.pack { scalar = i.scalar; negative = i.negative }
          }
        ~read_address:read_address.value
    in
    Always.(
      compile
        [ when_
            (push ^: pop)
            [ when_ push [ level <-- level.value +:. 1 ]
            ; when_ pop [ level <-- level.value -:. 1 ]
            ]
        ; when_ push [ write_address <-- write_address.value +:. 1 ]
        ; when_ pop [ read_address <-- read_address.value +:. 1 ]
        ]);
    let ({ scalar; negative } : Signal.t Scalar.t) = Scalar.Of_signal.unpack ram in
    { O_window.empty = level.value ==:. 0
    ; not_empty = level.value <>:. 0
    ; full = level.value ==:. stall_fifo_depth
    ; read_address = read_address.value
    ; write_address = write_address.value
    ; scalar
    ; negative
    }
  ;;

  let hierarchy_window scope ~window_index =
    let module Hier = Hierarchy.In_scope (I) (O_window) in
    Hier.hierarchical
      ~name:"stalled_fifo"
      ~instance:("sf_" ^ Int.to_string window_index)
      ~scope
      (create_window ~window_index)
  ;;

  let create scope (i : _ I.t) =
    let stalled_windows =
      List.init num_windows ~f:(fun window_index ->
        hierarchy_window scope i ~window_index)
    in
    let current_stalled_window = O_window.Of_signal.mux i.window stalled_windows in
    let affine_point_out =
      (Ram.create
         ~collision_mode:Write_before_read
         ~size:(num_windows * stall_fifo_depth)
         ~write_ports:
           [| { write_clock = i.clock
              ; write_address = i.window @: current_stalled_window.write_address
              ; write_data = i.affine_point
              ; write_enable = i.push
              }
           |]
         ~read_ports:
           [| { read_clock = i.clock
              ; read_address = i.window @: current_stalled_window.read_address
              ; read_enable = vdd
              }
           |]
         ()).(0)
    in
    let windows_with_stall = List.map stalled_windows ~f:(fun w -> w.not_empty) in
    { O.all_windows_have_stall = reduce ~f:( &: ) windows_with_stall
    ; some_windows_are_full =
        List.map stalled_windows ~f:(fun w -> w.full) |> reduce ~f:( |: )
    ; all_windows_are_empty =
        List.map stalled_windows ~f:(fun w -> w.empty) |> reduce ~f:( &: )
    ; current_window_has_stall = mux i.window windows_with_stall
    ; affine_point_out
    ; scalar_out = current_stalled_window.scalar
    ; scalar_out_valid = current_stalled_window.not_empty
    ; negative_out = current_stalled_window.negative
    }
  ;;

  let hierarchy scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~name:"stalled_point_fifos" ~scope create
  ;;
end
