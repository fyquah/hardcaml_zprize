open Base
open Core
open Hardcaml
open Signal

include struct
  open Pippenger
  module Controller = Controller
  module Scalar = Scalar
end

include struct
  open Hardcaml_xilinx
  module Dual_port_ram = Dual_port_ram
  module Ram_port = Ram_port
end

module Make (Config : sig
  module Top_config : Config.S

  val pipeline_depth : int
  val input_point_bits : int
end) =
struct
  open Config.Top_config
  open Config_utils.Make (Config.Top_config)

  module Scalar_config = struct
    let window_size_bits = max_window_size_bits
  end

  module Main_Controller =
    Controller.Make
      (struct
        let num_windows = num_windows
        let affine_point_bits = Config.input_point_bits
        let pipeline_depth = Config.pipeline_depth
        let log_stall_fifo_depth = controller_log_stall_fifo_depth
      end)
      (Scalar_config)

  module Scalar = Main_Controller.Scalar

  let scalar_sum_of_port_widths = Scalar.(fold ~init:0 port_widths ~f:( + ))

  module I = Main_Controller.I
  module O = Main_Controller.O

  let ctrl0_windows = num_windows / 2
  let ctrl1_windows = num_windows - ctrl0_windows

  module Controller0 =
    Controller.Make
      (struct
        let num_windows = ctrl0_windows
        let pipeline_depth = Config.pipeline_depth
        let log_stall_fifo_depth = controller_log_stall_fifo_depth
        let affine_point_bits = Config.input_point_bits
      end)
      (Scalar_config)

  module Controller1 =
    Controller.Make
      (struct
        let num_windows = ctrl1_windows
        let pipeline_depth = Config.pipeline_depth
        let log_stall_fifo_depth = controller_log_stall_fifo_depth
        let affine_point_bits = Config.input_point_bits
      end)
      (Scalar_config)

  let fifo_capacity = (* Native BRAM depth*) 512

  let create ~build_mode scope (i : _ I.t) : _ O.t =
    let ctrl0_scalar = Array.slice i.scalar 0 ctrl0_windows in
    let ctrl1_scalar =
      Array.slice i.scalar ctrl0_windows (ctrl0_windows + ctrl1_windows)
    in
    let packed_scalar0 =
      Array.map ctrl0_scalar ~f:Scalar.Of_signal.pack |> Signal.of_array
    in
    let packed_scalar1 =
      Array.map ctrl1_scalar ~f:Scalar.Of_signal.pack |> Signal.of_array
    in
    let spec = Reg_spec.create ~clock:i.clock () in
    let fifo_ready = wire 1 in
    let fifo0_rd = wire 1 in
    let fifo1_rd = wire 1 in
    let fifo0 =
      Hardcaml_xilinx.Fifo_sync.create
        ~build_mode
        ~showahead:true
        ~overflow_check:true
        ~underflow_check:true
        ~scope
        ~capacity:fifo_capacity
        ~clock:i.clock
        ~clear:i.clear
        ~wr:(i.scalar_valid &: fifo_ready)
        ~d:(i.last_scalar @: i.affine_point @: packed_scalar0)
        ~rd:fifo0_rd
        ()
    in
    let fifo1 =
      Hardcaml_xilinx.Fifo_sync.create
        ~build_mode
        ~showahead:true
        ~overflow_check:true
        ~underflow_check:true
        ~scope
        ~capacity:fifo_capacity
        ~clock:i.clock
        ~clear:i.clear
        ~wr:(i.scalar_valid &: fifo_ready)
        ~d:(i.last_scalar @: i.affine_point @: packed_scalar1)
        ~rd:fifo1_rd
        ()
    in
    fifo_ready <== (~:(fifo0.full) &: ~:(fifo1.full));
    let ctrl0 =
      let scalar_width = width packed_scalar0 in
      Controller0.hierarchy
        ~build_mode
        scope
        { Controller0.I.clock = i.clock
        ; clear = i.clear
        ; start = i.start
        ; scalar =
            sel_bottom fifo0.q scalar_width
            |> Signal.split_lsb ~part_width:scalar_sum_of_port_widths
            |> List.map ~f:Scalar.Of_signal.unpack
            |> List.to_array
        ; scalar_valid = ~:(fifo0.empty)
        ; last_scalar = msb fifo0.q
        ; affine_point =
            select fifo0.q (scalar_width + Config.input_point_bits - 1) scalar_width
        }
    in
    fifo0_rd <== ctrl0.scalar_read;
    let ctrl1 =
      let scalar_width = width packed_scalar1 in
      Controller1.hierarchy
        ~build_mode
        scope
        { Controller1.I.clock = i.clock
        ; clear = i.clear
        ; start = reg spec i.start
        ; scalar =
            sel_bottom fifo1.q scalar_width
            |> Signal.split_lsb ~part_width:scalar_sum_of_port_widths
            |> List.map ~f:Scalar.Of_signal.unpack
            |> List.to_array
        ; scalar_valid = ~:(fifo1.empty)
        ; last_scalar = msb fifo1.q
        ; affine_point =
            select fifo1.q (scalar_width + Config.input_point_bits - 1) scalar_width
        }
    in
    fifo1_rd <== ctrl1.scalar_read;
    let switch =
      reg_fb (Reg_spec.create ~clock:i.clock ~clear:i.start ()) ~f:(fun q -> ~:q) ~width:1
    in
    { Main_Controller.O.execute = mux2 switch ctrl0.execute ctrl1.execute
    ; done_ = ctrl0.done_ &: ctrl1.done_
    ; scalar_read = fifo_ready
    ; window =
        (let window_bits = num_bits_to_represent num_windows in
         mux2
           switch
           (uresize ctrl0.window window_bits)
           (uresize ctrl1.window window_bits +:. Array.length ctrl0_scalar))
    ; bucket = Scalar.Of_signal.mux2 switch ctrl0.bucket ctrl1.bucket
    ; adder_affine_point = mux2 switch ctrl0.adder_affine_point ctrl1.adder_affine_point
    ; bubble = mux2 switch ctrl0.bubble ctrl1.bubble
    }
  ;;

  let hierarchical ~build_mode scope =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~name:"full_controller" ~scope (create ~build_mode)
  ;;
end
