open Base
open Core
open Hardcaml
open Signal

include struct
  open Twisted_edwards_lib
  module Adder_config = Config
  module Mixed_add = Mixed_add
  module Mixed_add_precompute = Mixed_add_precompute
  module Num_bits = Num_bits
  module Xyzt = Xyzt
end

include struct
  open Pippenger
  module Controller = Controller
end

include struct
  open Hardcaml_xilinx
  module Dual_port_ram = Dual_port_ram
  module Ram_port = Ram_port
end

module Make (Config : Config.S) = struct
  open Config
  open Config_utils.Make (Config)

  let precompute = true

  module Num_bits = struct
    let num_bits = field_bits
  end

  module Mixed_add = Mixed_add_precompute.Make (Num_bits)
  module Scalar_transformation = Scalar_transformation.Make (Config) (Num_bits)

  let adder_config = force Adder_config.For_bls12_377.with_barrett_reduction_full
  let adder_latency = Mixed_add.latency adder_config

  (* for now, assert that there are 1 or 2 window sizes *)
  (*let () = assert((max_window_size_bits = min_window_size_bits) || (max_window_size_bits = min_window_size_bits + 1))
  let max_window_indices = Array.filter_mapi window_size_bits ~f:(fun i v -> if (v = max_window_size_bits) then Some i else None)
  let is_max_window window = 
    (List.map max_window_indices ~f:(fun ind -> window ==:. ind)) |> reduce ~f:(|:)*)
  let max_ram_address window =
    (* vivado should reduce this for our values? *)
    mux
      window
      (List.init num_windows ~f:(fun i ->
         of_int (num_buckets i - 1) ~width:max_window_size_bits))
  ;;

  let num_windows = num_windows
  let num_result_points = List.(init num_windows ~f:num_buckets |> fold ~init:0 ~f:( + ))
  let max_window_size_bits = max_window_size_bits
  let input_point_bits = Mixed_add.Xyt.(fold port_widths ~init:0 ~f:( + ))
  let result_point_bits = Mixed_add.Xyzt.(fold port_widths ~init:0 ~f:( + ))
  let ram_read_latency = 3
  let ram_lookup_latency = 3
  let ram_write_latency = 3

  module Full_controller = Full_controller.Make (struct
    module Top_config = Config

    let pipeline_depth =
      Int.round_up
        (adder_latency + ram_lookup_latency + ram_read_latency + ram_write_latency + 2)
        ~to_multiple_of:2
      / 2
    ;;

    let input_point_bits = input_point_bits
  end)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; scalar : 'a [@bits scalar_bits]
      ; scalar_valid : 'a
      ; last_scalar : 'a
      ; input_point : 'a Mixed_add.Xyt.t [@rtl_prefix "i_"]
      ; result_point_ready : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { result_point : 'a Mixed_add.Xyzt.t [@rtl_prefix "o_"]
      ; result_point_valid : 'a
      ; last_result_point : 'a
      ; scalar_and_input_point_ready : 'a
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:true]
  end

  module State = struct
    type t =
      | Init_to_identity
      | Idle
      | Working
      | Read_result
      | Wait_for_done_reading
    [@@deriving sexp_of, compare, enumerate]
  end

  let identity_point_for_ram =
    if precompute
    then (
      let point =
        Twisted_edwards_model_lib.Twisted_edwards_curve.to_fpga_internal_representation
          { x = Z.zero; y = Z.one; t = Z.zero; z = Z.one }
      in
      { Mixed_add.Xyzt.x = Signal.of_z point.x ~width:Config.field_bits
      ; y = Signal.of_z point.y ~width:Config.field_bits
      ; t = Signal.of_z point.t ~width:Config.field_bits
      ; z = Signal.of_z point.z ~width:Config.field_bits
      })
    else Mixed_add.Xyzt.Of_signal.of_ints { x = 0; y = 1; t = 0; z = 1 }
  ;;

  let create
    ~build_mode
    scope
    { I.clock; clear; scalar; scalar_valid; last_scalar; input_point; result_point_ready }
    =
    let ( -- ) = Scope.naming scope in
    let open Always in
    (* preprocess the scalar *)
    let controller_ready = wire 1 in
    let { Scalar_transformation.O.scalar
        ; scalar_negatives
        ; scalar_valid
        ; last_scalar
        ; input_point
        ; scalar_and_input_point_ready
        }
      =
      Scalar_transformation.hierarchical
        scope
        { clock
        ; clear
        ; scalar
        ; scalar_valid
        ; last_scalar
        ; input_point
        ; scalar_and_input_point_ready = controller_ready
        }
    in
    let spec = Reg_spec.create ~clock () in
    let spec_with_clear = Reg_spec.create ~clear ~clock () in
    let sm = State_machine.create (module State) spec_with_clear in
    let ctrl_start = Variable.reg spec_with_clear ~width:1 in
    let ctrl =
      Full_controller.hierarchical
        scope
        { Full_controller.I.clock
        ; clear
        ; start = ctrl_start.value
        ; scalar =
            Array.map2_exn scalar scalar_negatives ~f:(fun scalar negative ->
              { Full_controller.Scalar.scalar; negative })
        ; scalar_valid = (scalar_valid &: sm.is Working) -- "scalar_valid"
        ; last_scalar
        ; affine_point = Mixed_add.Xyt.Of_signal.pack input_point
        }
    in
    let ctrl_affine_point_as_xyt =
      Mixed_add.Xyt.Of_signal.unpack ctrl.adder_affine_point
    in
    let adder_valid_in = ctrl.execute &: ~:(ctrl.bubble) in
    ignore (sm.current -- "STATE" : Signal.t);
    let adder_p3 = wire result_point_bits -- "adder_p3" in
    let window_address =
      Variable.reg spec_with_clear ~width:(num_bits_to_represent num_windows)
    in
    let bucket_address = Variable.reg spec_with_clear ~width:max_window_size_bits in
    let adder_valid_out = wire 1 in
    let fifo_q_has_space = wire 1 in
    let port_a_q, port_b_q =
      List.init num_windows ~f:(fun window ->
        let num_buckets = num_buckets window in
        let address_bits = Int.ceil_log2 num_buckets in
        let ctrl_window_en = ctrl.window ==:. window in
        (* To support write before read mode in URAM, writes must happen on port
           A and reads on port B. *)
        Dual_port_ram.create
          ~read_latency:ram_read_latency
          ~arch:Ultraram
          ~build_mode
          ~clock
          ~clear
          ~size:num_buckets
          ~port_a:
            (let pre_transform_address =
               sel_bottom
                 (pipeline
                    ~n:ram_lookup_latency
                    spec
                    (mux2
                       (sm.is Read_result)
                       bucket_address.value
                       (pipeline
                          ~n:(ram_read_latency + adder_latency + ram_write_latency)
                          spec
                          ctrl.bucket.scalar
                       -- "ctrl.bucket")))
                 address_bits
             in
             let port =
               { Ram_port.write_enable =
                   pipeline
                     ~n:
                       (ram_lookup_latency
                       + ram_read_latency
                       + adder_latency
                       + ram_write_latency)
                     spec
                     ((ctrl.execute &: ~:(ctrl.bubble) &: ctrl_window_en) -- "port_a_we")
               ; read_enable =
                   pipeline
                     ~n:ram_lookup_latency
                     spec
                     ((sm.is Read_result
                      &: (window_address.value ==:. window)
                      &: fifo_q_has_space)
                     -- "port_a_re")
               ; data = pipeline ~n:ram_write_latency spec (adder_p3 -- "port_a_d")
               ; address = scalar_to_ram_index (module Signal) pre_transform_address
               }
             in
             Ram_port.(
               iter2 port port_names ~f:(fun s n ->
                 ignore
                   (s -- ("window" ^ Int.to_string window ^ "$ram_a$" ^ n) : Signal.t)));
             port)
          ~port_b:
            (let pre_transform_address =
               pipeline
                 ~n:ram_lookup_latency
                 spec
                 (mux2
                    (sm.is Read_result)
                    (sel_bottom bucket_address.value address_bits -- "bucket.address")
                    (sel_bottom ctrl.bucket.scalar address_bits -- "ctrl.bucket"))
             in
             let port =
               { Ram_port.write_enable =
                   pipeline
                     ~n:ram_lookup_latency
                     spec
                     ((sm.is Read_result
                      &: (window_address.value ==:. window)
                      &: fifo_q_has_space)
                     -- "port_b_we")
               ; read_enable =
                   pipeline
                     ~n:ram_lookup_latency
                     spec
                     ((ctrl.execute &: ~:(ctrl.bubble) &: ctrl_window_en) -- "port_b_re")
               ; data = Mixed_add.Xyzt.Of_signal.pack identity_point_for_ram
               ; address = scalar_to_ram_index (module Signal) pre_transform_address
               }
             in
             Ram_port.(
               iter2 port port_names ~f:(fun s n ->
                 ignore
                   (s -- ("window" ^ Int.to_string window ^ "$ram_b$" ^ n) : Signal.t)));
             port)
          ())
      |> List.unzip
    in
    List.iteri port_a_q ~f:(fun i port ->
      ignore (port -- ("window" ^ Int.to_string i ^ "$ram_a$q") : Signal.t));
    List.iteri port_b_q ~f:(fun i port ->
      ignore (port -- ("window" ^ Int.to_string i ^ "$ram_b$q") : Signal.t));
    let adder =
      let n = ram_lookup_latency + ram_read_latency in
      Mixed_add.hierarchical
        scope
        ~config:adder_config
        { clock
        ; valid_in = pipeline spec adder_valid_in ~n
        ; p1 =
            Mixed_add.Xyzt.Of_signal.unpack (mux (pipeline spec ctrl.window ~n) port_b_q)
        ; p2 = Mixed_add.Xyt.Of_signal.(pipeline spec ctrl_affine_point_as_xyt ~n)
        ; subtract = pipeline spec ctrl.bucket.negative ~n
        }
    in
    adder_valid_out <== adder.valid_out;
    ignore (adder_valid_out -- "adder_valid_out" : Signal.t);
    adder_p3 <== Mixed_add.Xyzt.Of_signal.pack adder.p3;
    (* State machine for flow control. *)
    let wait_count =
      Variable.reg
        spec_with_clear
        ~width:
          (num_bits_to_represent
             (ram_lookup_latency + ram_read_latency + adder_latency + ram_write_latency))
    in
    let last_scalar_l = Variable.reg spec_with_clear ~width:1 in
    let last_result_point = wire 1 in
    let done_l = Variable.reg spec_with_clear ~width:1 in
    let finished = Variable.wire ~default:gnd in
    ignore (finished.value -- "finished" : Signal.t);
    compile
      [ sm.switch
          [ (* We initialize the RAM to identity by doing a "fake" ram read in the Read_result state. *)
            ( Init_to_identity
            , [ bucket_address <--. num_buckets 0 - 1
              ; window_address <--. 0
              ; sm.set_next Read_result
              ] )
          ; ( Idle
            , [ bucket_address <--. 0
              ; window_address <--. 0
              ; wait_count <--. 0
              ; done_l <--. 0
              ; finished <-- gnd
              ; last_scalar_l <-- gnd
              ; when_ scalar_valid [ ctrl_start <-- vdd; sm.set_next Working ]
              ] )
          ; ( Working
            , [ ctrl_start <-- gnd
              ; last_scalar_l <-- (last_scalar_l.value |: last_scalar)
              ; when_ (last_scalar_l.value &: ctrl.done_) [ done_l <-- vdd ]
              ; when_ done_l.value [ wait_count <-- wait_count.value +:. 1 ]
              ; when_
                  (* After finishing we still need for results to flush from the adder *)
                  (wait_count.value
                  ==:. ram_lookup_latency
                       + adder_latency
                       + ram_read_latency
                       + ram_write_latency
                       - 1)
                  [ window_address <--. 0
                  ; bucket_address <--. num_buckets 0 - 1
                  ; sm.set_next Read_result
                  ]
              ] )
          ; ( Read_result
            , [ when_
                  fifo_q_has_space
                  [ bucket_address <-- bucket_address.value -:. 1
                  ; when_
                      (bucket_address.value ==:. 1)
                      [ window_address <-- window_address.value -- "window_address" +:. 1
                      ; bucket_address <-- max_ram_address window_address.value
                      ; when_
                          (window_address.value ==:. num_windows - 1)
                          [ bucket_address <--. 0
                          ; finished <-- vdd
                          ; sm.set_next Wait_for_done_reading
                          ]
                      ]
                  ]
              ] )
          ; ( Wait_for_done_reading
            , [ when_
                  (~:(done_l.value) |: (last_result_point &: result_point_ready))
                  [ sm.set_next Idle ]
              ] )
          ]
      ];
    (* Put the output points through a FIFO so that downstream can backpressure
       and we hold off on reading points from RAM. *)
    let fifo_capacity = 16 in
    let fifo_q =
      let wr =
        pipeline
          spec
          ~n:(ram_lookup_latency + ram_read_latency)
          (done_l.value &: sm.is Read_result &: fifo_q_has_space -- "fifo_q_has_space")
        -- "fifo_wr"
      in
      let d =
        pipeline spec finished.value ~n:(ram_lookup_latency + ram_read_latency)
        @: (mux
              (pipeline
                 spec
                 window_address.value
                 ~n:(ram_lookup_latency + ram_read_latency))
              port_a_q
           -- "fifo_d")
      in
      let rd = result_point_ready -- "fifo_rd" in
      Fifo.create_showahead_with_extra_reg
        ~overflow_check:true
        ~underflow_check:true
        ~scope
        ~capacity:fifo_capacity
        ~clock
        ~clear
        ~wr
        ~d
        ~rd
        ()
    in
    fifo_q_has_space
    <== (fifo_q.used <:. fifo_capacity - ram_lookup_latency - ram_read_latency - 2);
    last_result_point <== (msb fifo_q.q &: ~:(fifo_q.empty));
    controller_ready <== (ctrl.scalar_read &: sm.is Working);
    { O.result_point = Mixed_add.Xyzt.Of_signal.unpack (lsbs fifo_q.q)
    ; result_point_valid = ~:(fifo_q.empty)
    ; last_result_point
    ; scalar_and_input_point_ready
    }
  ;;

  let hierarchical ?instance ~build_mode scope =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ?instance ~name:"top" ~scope (create ~build_mode)
  ;;
end
