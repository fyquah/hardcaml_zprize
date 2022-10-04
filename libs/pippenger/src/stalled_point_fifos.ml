open Base
open Hardcaml
open Signal

module Make (Config : Config.S) (Scalar_config : Scalar.Scalar_config.S) = struct
  open Config
  module Scalar = Scalar.Make (Scalar_config)

  let log_num_windows = Int.ceil_log2 num_windows
  let stall_fifo_depth = 1 lsl log_stall_fifo_depth

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; push : 'a
      ; scalar : 'a Scalar.t [@rtlprefix "i_"]
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
      ; scalar_out : 'a Scalar.t
      ; scalar_out_valid : 'a
      ; scalars_out : 'a Scalar.t array [@length num_windows]
      ; scalars_out_valid : 'a array [@length num_windows]
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
      ; scalar : 'a Scalar.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Var = Always.Variable

  let create_window scope (i : _ I.t) ~window_index =
    let ( -- ) = Scope.naming scope in
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let enable = i.window ==:. window_index in
    let read_address = Var.reg spec ~width:log_stall_fifo_depth in
    let write_address = Var.reg spec ~width:log_stall_fifo_depth in
    let level = Var.reg spec ~width:(log_stall_fifo_depth + 1) in
    ignore (level.value -- "LEVEL" : Signal.t);
    let full = Var.reg spec ~width:1 in
    let not_empty = Var.reg spec ~width:1 in
    let empty = Var.reg (Reg_spec.override spec ~clear_to:vdd) ~width:1 in
    let push = i.push &: enable in
    let pop = i.pop &: enable in
    let ram =
      memory
        stall_fifo_depth
        ~write_port:
          { write_clock = i.clock
          ; write_address = write_address.value
          ; write_enable = push
          ; write_data = Scalar.Of_signal.pack i.scalar
          }
        ~read_address:read_address.value
    in
    let open Always in
    let update_status level =
      [ empty <-- (level ==:. 0)
      ; not_empty <-- (level <>:. 0)
      ; full <-- (level ==:. stall_fifo_depth)
      ]
      |> proc
    in
    compile
      [ update_status level.value
      ; when_
          (push ^: pop)
          [ when_ push [ level <-- level.value +:. 1; update_status (level.value +:. 1) ]
          ; when_ pop [ level <-- level.value -:. 1; update_status (level.value -:. 1) ]
          ]
      ; when_ push [ write_address <-- write_address.value +:. 1 ]
      ; when_ pop [ read_address <-- read_address.value +:. 1 ]
      ];
    { O_window.empty = empty.value
    ; not_empty = not_empty.value
    ; full = full.value
    ; read_address = read_address.value
    ; write_address = write_address.value
    ; scalar = Scalar.Of_signal.unpack ram
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

  let create ~build_mode scope (i : _ I.t) =
    let stalled_windows =
      List.init num_windows ~f:(fun window_index ->
        hierarchy_window scope i ~window_index)
    in
    let current_stalled_window = O_window.Of_signal.mux i.window stalled_windows in
    let affine_point_out =
      let write_address = i.window @: current_stalled_window.write_address in
      let read_address = i.window @: current_stalled_window.read_address in
      let address = mux2 i.push write_address read_address in
      let w = width i.affine_point in
      let data_a, data_b =
        let a, b = i.affine_point.:[w - 1, w / 2], i.affine_point.:[(w / 2) - 1, 0] in
        if w % 2 = 0 then a, b else a, ue b
      in
      let qa, qb =
        Hardcaml_xilinx.True_dual_port_ram.create
          ~arch:(Blockram Write_before_read)
          ~build_mode
          ()
          ~clock_a:i.clock
          ~clock_b:i.clock
          ~clear_a:gnd
          ~clear_b:gnd
          ~size:(num_windows * stall_fifo_depth * 2)
          ~port_a:
            { address = address @: gnd
            ; data = data_a
            ; read_enable = ~:(i.push)
            ; write_enable = i.push
            }
          ~port_b:
            { address = address @: vdd
            ; data = data_b
            ; read_enable = ~:(i.push)
            ; write_enable = i.push
            }
      in
      if w % 2 = 0 then qa @: qb else qa @: lsbs qb
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
    ; scalars_out = List.map stalled_windows ~f:(fun w -> w.scalar) |> List.to_array
    ; scalars_out_valid =
        List.map stalled_windows ~f:(fun w -> w.not_empty) |> List.to_array
    }
  ;;

  let hierarchy ~build_mode scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~name:"stalled_point_fifos" ~scope (create ~build_mode)
  ;;
end
