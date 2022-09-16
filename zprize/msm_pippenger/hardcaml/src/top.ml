open Base
open Core
open Hardcaml
open Signal

include struct
  open Twisted_edwards_lib
  module Adder_config = Config
  module Mixed_add = Mixed_add
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

  module Mixed_add = Mixed_add.Make (struct
    let num_bits = field_bits
  end)

  let adder_config = force Adder_config.For_bls12_377.with_barrett_reduction
  let adder_latency = Mixed_add.latency adder_config

  (* Integer divison so the last window might be slightly larger than the others. *)
  let num_windows = scalar_bits / window_size_bits
  let last_window_size_bits = scalar_bits - (window_size_bits * (num_windows - 1))

  let num_result_points =
    ((num_windows - 1) lsl window_size_bits) + (1 lsl last_window_size_bits) - num_windows
  ;;

  let input_point_bits = Mixed_add.Xyt.(fold port_widths ~init:0 ~f:( + ))
  let result_point_bits = Mixed_add.Xyzt.(fold port_widths ~init:0 ~f:( + ))
  let ram_read_latency = Config.ram_read_latency
  let ram_lookup_latency = 3
  let ram_write_latency = 3

  module Controller = Controller.Make (struct
    let window_size_bits = last_window_size_bits
    let num_windows = num_windows
    let affine_point_bits = input_point_bits

    let pipeline_depth =
      Int.round_up
        (adder_latency + ram_lookup_latency + ram_read_latency + ram_write_latency + 2)
        ~to_multiple_of:2
      / 2
    ;;

    let log_stall_fifo_depth = controller_log_stall_fifo_depth
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

  module Adder = struct
    include Mixed_add

    let hierarchical ~config scope =
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical ~name:"adder" ~scope (create ~config)
    ;;
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

  let identity_point = Mixed_add.Xyzt.Of_signal.of_ints { x = 0; y = 1; t = 0; z = 1 }

  let create
    ~build_mode
    scope
    { I.clock; clear; scalar; scalar_valid; last_scalar; input_point; result_point_ready }
    =
    let ( -- ) = Scope.naming scope in
    let open Always in
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
    let spec = Reg_spec.create ~clear ~clock () in
    let sm = State_machine.create (module State) spec in
    let ctrl_start = Variable.reg spec ~width:1 in
    let ctrl =
      Controller.hierarchy
        scope
        { Controller.I.clock
        ; clear
        ; start = ctrl_start.value
        ; scalar
        ; scalar_valid = scalar_valid &: sm.is Working
        ; last_scalar
        ; affine_point = Mixed_add.Xyt.Of_signal.pack input_point
        }
    in
    let ctrl_affine_point_as_xyt = Adder.Xyt.Of_signal.unpack ctrl.adder_affine_point in
    let adder_valid_in = ctrl.execute &: ~:(ctrl.bubble) in
    ignore (sm.current -- "STATE" : Signal.t);
    let adder_p3 = wire result_point_bits in
    let window_address = Variable.reg spec ~width:(num_bits_to_represent num_windows) in
    let bucket_address = Variable.reg spec ~width:last_window_size_bits in
    let adder_valid_out = wire 1 in
    let fifo_q_has_space = wire 1 in
    let port_a_q, port_b_q =
      List.init num_windows ~f:(fun window ->
        let address_bits =
          if window = num_windows - 1 then last_window_size_bits else window_size_bits
        in
        let ctrl_window_en = ctrl.window ==:. window in
        (* To support write before read mode in URAM, writes must happen on port
           A and reads on port B. *)
        Dual_port_ram.create
          ~read_latency:ram_read_latency
          ~arch:Ultraram
          ~build_mode
          ~clock
          ~clear
          ~size:
            (if window = num_windows - 1
            then 1 lsl last_window_size_bits
            else 1 lsl window_size_bits)
          ~port_a:
            (let port =
               { Ram_port.write_enable =
                   mux2
                     (sm.is Init_to_identity)
                     vdd
                     (pipeline ~n:ram_write_latency spec adder_valid_out
                     &: pipeline
                          ~n:
                            (ram_lookup_latency
                            + ram_read_latency
                            + adder_latency
                            + ram_write_latency)
                          spec
                          (ctrl.execute &: ~:(ctrl.bubble) &: ctrl_window_en))
               ; read_enable =
                   pipeline
                     ~n:ram_lookup_latency
                     spec
                     (sm.is Read_result
                     &: (window_address.value ==:. window)
                     &: fifo_q_has_space)
               ; data =
                   mux2
                     (sm.is Init_to_identity)
                     (Mixed_add.Xyzt.Of_signal.pack identity_point)
                     (pipeline ~n:ram_write_latency spec adder_p3)
               ; address =
                   sel_bottom
                     (mux2
                        (sm.is Init_to_identity)
                        bucket_address.value
                        (mux2
                           (pipeline ~n:ram_lookup_latency spec (sm.is Read_result))
                           (pipeline ~n:ram_lookup_latency spec bucket_address.value)
                           (pipeline
                              ~n:
                                (ram_lookup_latency
                                + ram_read_latency
                                + adder_latency
                                + ram_write_latency)
                              spec
                              ctrl.bucket)))
                     address_bits
               }
             in
             Ram_port.(
               iter2 port port_names ~f:(fun s n ->
                 ignore
                   (s -- ("window" ^ Int.to_string window ^ "$ram_a$" ^ n) : Signal.t)));
             port)
          ~port_b:
            (let port =
               { Ram_port.write_enable = gnd
               ; read_enable =
                   pipeline
                     ~n:ram_lookup_latency
                     spec
                     (ctrl.execute &: ~:(ctrl.bubble) &: ctrl_window_en)
               ; data = zero result_point_bits
               ; address =
                   pipeline
                     ~n:ram_lookup_latency
                     spec
                     (sel_bottom ctrl.bucket address_bits)
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
      Adder.hierarchical
        scope
        ~config:adder_config
        { clock
        ; clear
        ; valid_in =
            pipeline spec adder_valid_in ~n:(ram_lookup_latency + ram_read_latency)
        ; p1 =
            Mixed_add.Xyzt.Of_signal.unpack
              (mux
                 (pipeline spec ctrl.window ~n:(ram_lookup_latency + ram_read_latency))
                 port_b_q)
        ; p2 =
            Mixed_add.Xyt.Of_signal.(
              pipeline
                spec
                (mux2 ctrl.bubble (of_int 0) ctrl_affine_point_as_xyt)
                ~n:(ram_lookup_latency + ram_read_latency))
        }
    in
    adder_valid_out <== adder.valid_out;
    ignore (adder_valid_out -- "adder_valid_out" : Signal.t);
    adder_p3 <== Mixed_add.Xyzt.Of_signal.pack adder.p3;
    (* State machine for flow control. *)
    let wait_count =
      Variable.reg
        spec
        ~width:
          (num_bits_to_represent
             (ram_lookup_latency + ram_read_latency + adder_latency + ram_write_latency))
    in
    let last_scalar_l = Variable.reg spec ~width:1 in
    let last_result_point = wire 1 in
    let done_l = Variable.reg spec ~width:1 in
    let finished = Variable.wire ~default:gnd in
    ignore (finished.value -- "finished" : Signal.t);
    compile
      [ sm.switch
          [ ( Init_to_identity
            , [ done_l <-- gnd
              ; finished <-- gnd
              ; last_scalar_l <-- gnd
              ; bucket_address <-- bucket_address.value -- "bucket_address" +:. 1
              ; when_
                  (bucket_address.value ==:. (1 lsl last_window_size_bits) - 1)
                  [ sm.set_next Idle ]
              ] )
          ; ( Idle
            , [ bucket_address <--. 0
              ; window_address <--. 0
              ; ctrl_start <-- vdd
              ; sm.set_next Working
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
                  ; bucket_address <--. (1 lsl window_size_bits) - 1
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
                      ; if_
                          (window_address.value ==:. num_windows - 2)
                          [ bucket_address <--. (1 lsl last_window_size_bits) - 1 ]
                          [ bucket_address <--. (1 lsl window_size_bits) - 1 ]
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
            , [ when_ last_result_point [ sm.set_next Init_to_identity ] ] )
          ]
      ];
    (* Put the output points through a FIFO so that downstream can backpressure
       and we hold off on reading points from RAM. *)
    let fifo_capacity = 32 in
    let fifo_q =
      let wr =
        pipeline
          spec
          ~n:(ram_lookup_latency + ram_read_latency)
          (sm.is Read_result &: fifo_q_has_space -- "fifo_q_has_space")
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
      Fifo.create_showahead_from_classic
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
    { O.result_point = Mixed_add.Xyzt.Of_signal.unpack (lsbs fifo_q.q)
    ; result_point_valid = ~:(fifo_q.empty)
    ; last_result_point
    ; scalar_and_input_point_ready = ctrl.scalar_read
    }
  ;;

  let hierarchical ?instance ~build_mode scope =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ?instance ~name:"top" ~scope (create ~build_mode)
  ;;
end
