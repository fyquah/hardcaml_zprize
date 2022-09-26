open Base
open Core
open Hardcaml
open Signal

include struct
  open Pippenger
  module Controller = Controller
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

  (* Integer divison so the last window might be slightly larger than the others. *)
  let num_windows = scalar_bits / window_size_bits
  let last_window_size_bits = scalar_bits - (window_size_bits * (num_windows - 1))

  module Main_Controller = Controller.Make (struct
    let window_size_bits = last_window_size_bits
    let num_windows = num_windows
    let affine_point_bits = Config.input_point_bits
    let pipeline_depth = Config.pipeline_depth
    let log_stall_fifo_depth = controller_log_stall_fifo_depth
  end)

  module I = Main_Controller.I
  module O = Main_Controller.O

  let ctrl0_windows = num_windows / 3
  let ctrl1_windows = ctrl0_windows
  let ctrl2_windows = num_windows - ctrl0_windows - ctrl1_windows

  module Controller0 = Controller.Make (struct
    let window_size_bits = last_window_size_bits
    let num_windows = ctrl0_windows
    let pipeline_depth = Config.pipeline_depth
    let log_stall_fifo_depth = controller_log_stall_fifo_depth
    let affine_point_bits = Config.input_point_bits
  end)

  module Controller1 = Controller.Make (struct
    let window_size_bits = last_window_size_bits
    let num_windows = ctrl1_windows
    let pipeline_depth = Config.pipeline_depth
    let log_stall_fifo_depth = controller_log_stall_fifo_depth
    let affine_point_bits = Config.input_point_bits
  end)

  module Controller2 = Controller.Make (struct
    let window_size_bits = last_window_size_bits
    let num_windows = ctrl2_windows
    let pipeline_depth = Config.pipeline_depth
    let log_stall_fifo_depth = controller_log_stall_fifo_depth
    let affine_point_bits = Config.input_point_bits
  end)

  let create scope (i : _ I.t) : _ O.t =
    let ctrl0_scalar = Array.slice i.scalar 0 ctrl0_windows in
    let ctrl1_scalar =
      Array.slice i.scalar ctrl0_windows (ctrl0_windows + ctrl1_windows)
    in
    let ctrl2_scalar =
      Array.slice
        i.scalar
        (ctrl0_windows + ctrl1_windows)
        (ctrl0_windows + ctrl1_windows + ctrl2_windows)
    in
    let spec = Reg_spec.create ~clock:i.clock () in
    let fifo_ready = wire 1 in
    let fifo0_rd = wire 1 in
    let fifo1_rd = wire 1 in
    let fifo2_rd = wire 1 in
    let fifo0 =
      Fifo.create_showahead_with_extra_reg
        ~overflow_check:true
        ~underflow_check:true
        ~scope
        ~capacity:64
        ~clock:i.clock
        ~clear:i.clear
        ~wr:(i.scalar_valid &: fifo_ready)
        ~d:(i.last_scalar @: i.affine_point @: Signal.of_array ctrl0_scalar)
        ~rd:fifo0_rd
        ()
    in
    let fifo1 =
      Fifo.create_showahead_with_extra_reg
        ~overflow_check:true
        ~underflow_check:true
        ~scope
        ~capacity:64
        ~clock:i.clock
        ~clear:i.clear
        ~wr:(i.scalar_valid &: fifo_ready)
        ~d:(i.last_scalar @: i.affine_point @: Signal.of_array ctrl1_scalar)
        ~rd:fifo1_rd
        ()
    in
    let fifo2 =
      Fifo.create_showahead_with_extra_reg
        ~overflow_check:true
        ~underflow_check:true
        ~scope
        ~capacity:64
        ~clock:i.clock
        ~clear:i.clear
        ~wr:(i.scalar_valid &: fifo_ready)
        ~d:(i.last_scalar @: i.affine_point @: Signal.of_array ctrl2_scalar)
        ~rd:fifo2_rd
        ()
    in
    fifo_ready <== (~:(fifo0.full) &: ~:(fifo1.full));
    let ctrl0 =
      let scalar_width = width (Signal.of_array ctrl0_scalar) in
      Controller0.hierarchy
        scope
        { Controller0.I.clock = i.clock
        ; clear = i.clear
        ; start = i.start
        ; scalar =
            sel_bottom fifo0.q scalar_width
            |> Signal.split_lsb ~part_width:last_window_size_bits
            |> List.to_array
        ; scalar_valid = ~:(fifo0.empty)
        ; last_scalar = msb fifo0.q
        ; affine_point =
            select fifo0.q (scalar_width + Config.input_point_bits - 1) scalar_width
        }
    in
    fifo0_rd <== ctrl0.scalar_read;
    let ctrl1 =
      let scalar_width = width (Signal.of_array ctrl1_scalar) in
      Controller1.hierarchy
        scope
        { Controller1.I.clock = i.clock
        ; clear = i.clear
        ; start = reg spec i.start
        ; scalar =
            sel_bottom fifo1.q scalar_width
            |> Signal.split_lsb ~part_width:last_window_size_bits
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
    ; bucket = mux2 switch ctrl0.bucket ctrl1.bucket
    ; adder_affine_point = mux2 switch ctrl0.adder_affine_point ctrl1.adder_affine_point
    ; bubble = mux2 switch ctrl0.bubble ctrl1.bubble
    }
  ;;

  let hierarchical scope =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~name:"full_controller" ~scope create
  ;;
end
