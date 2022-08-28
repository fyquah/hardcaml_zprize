open Base
open Hardcaml
open Signal

include struct
  open Twisted_edwards_lib
  module Adder_config = Mixed_add.Config
  module Mixed_add = Mixed_add
  module Num_bits = Num_bits
end

include struct
  open Pippenger
  module Controller = Controller
end

module Make (Config : Config.S) = struct
  open Config

  module Mixed_add = Mixed_add.Make (struct
    let num_bits = field_bits
  end)

  let adder_config = force Adder_config.For_bls12_377.with_barrett_reduction
  let adder_latency = Mixed_add.latency adder_config

  (* Integer divison so the last window might be slightly larger than the others. *)
  let num_windows = scalar_bits / window_size_bits
  let last_window_size_bits = scalar_bits - (window_size_bits * (num_windows - 1))
  let input_point_bits = Mixed_add.Xyt.(fold port_widths ~init:0 ~f:( + ))
  let result_point_bits = Mixed_add.Xyzt.(fold port_widths ~init:0 ~f:( + ))

  module Controller = Controller.Make (struct
    let window_size_bits = last_window_size_bits
    let num_windows = num_windows
    let affine_point_bits = input_point_bits
    let pipeline_depth = adder_latency
    let log_stall_fifo_depth = controller_log_stall_fifo_depth
  end)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; scalar : 'a [@bits scalar_bits]
      ; scalar_valid : 'a
      ; last_scalar : 'a
      ; input_point : 'a [@bits input_point_bits]
      ; result_point_ready : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { result_point : 'a [@bits result_point_bits]
      ; result_point_valid : 'a
      ; scalar_and_input_point_ready : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create
    scope
    { I.clock
    ; clear
    ; start
    ; scalar
    ; scalar_valid
    ; last_scalar
    ; input_point
    ; result_point_ready
    }
    =
    (* We want to split our [scalar_bits] input scalar into an array of windows.
       The last one might be larger. *)
    let scalar =
      Array.init num_windows ~f:(fun i ->
        if i = num_windows - 1
        then sel_top scalar last_window_size_bits
        else
          uresize
            scalar.:+[window_size_bits * i, Some window_size_bits]
            last_window_size_bits)
    in
    let ctrl =
      Controller.hierarchy
        scope
        { Controller.I.clock
        ; clear
        ; start
        ; scalar
        ; scalar_valid
        ; last_scalar
        ; affine_point = input_point
        }
    in
    let adder =
      Mixed_add.hierarchical
        scope
        ~config
        { clock; enable = vdd; valid_in; data_in0; data_in1; data_in1_z_squared }
    in
    let port_a, port_b =
      List.init num_windows ~f:(fun window ->
        let q =
          Ram.create
            ~collision_mode:Write_before_read
            ~size:(1 lsl window_size_bits)
            ~read_ports:
              [| { read_clock = clock; read_address = ctrl.bucket; read_enable = vdd }
               ; { read_clock = clock
                 ; read_address = i.bucket_address
                 ; read_enable = i.bucket_read_enable
                 }
              |]
            ~write_ports:
              [| { write_clock = clock
                 ; write_address = dp.bucket
                 ; write_enable = dp.write_enable &: (dp.window ==:. window)
                 ; write_data = dp.result
                 }
              |]
            ()
        in
        q.(0), q.(1))
      |> List.unzip
    in
    (* TODO bdevlin: Make sure we read form large to small *)
    O.Of_signal.of_int 0
  ;;

  let hierarchical ~config ~p scope i =
    let module H = Hierarchy.In_scope (I) (O) in
    Hierarchical.hierarchical ~name:"top" ~scope create
  ;;
end
