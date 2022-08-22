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
      ; scalar : 'a [@bits window_size_bits]
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
      ; affine_point_out : 'a [@bits affine_point_bits]
      ; scalar_out : 'a [@bits window_size_bits]
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
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Var = Always.Variable

  let create_window _scope (i : _ I.t) ~window_index =
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let enable = i.window ==:. window_index in
    let read_address = Var.reg spec ~width:log_stall_fifo_depth in
    let write_address = Var.reg spec ~width:log_stall_fifo_depth in
    let level = Var.reg spec ~width:(log_stall_fifo_depth + 1) in
    let push = i.push &: enable in
    let pop = i.pop &: enable in
    let scalar =
      memory
        stall_fifo_depth
        ~write_port:
          { write_clock = i.clock
          ; write_address = write_address.value
          ; write_enable = push
          ; write_data = i.scalar
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
    { O_window.empty = level.value ==:. 0
    ; not_empty = level.value <>:. 0
    ; full = level.value ==:. stall_fifo_depth
    ; read_address = read_address.value
    ; write_address = write_address.value
    ; scalar
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
              ; read_enable = i.pop
              }
           |]
         ()).(0)
    in
    { O.all_windows_have_stall =
        List.map stalled_windows ~f:(fun w -> w.not_empty) |> reduce ~f:( &: )
    ; some_windows_are_full =
        List.map stalled_windows ~f:(fun w -> w.full) |> reduce ~f:( |: )
    ; affine_point_out
    ; scalar_out = current_stalled_window.scalar
    }
  ;;

  let hierarchy scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~name:"stalled_point_fifos" ~scope create
  ;;
end
