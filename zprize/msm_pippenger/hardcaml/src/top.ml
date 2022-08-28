open Base
open Hardcaml
open Signal

include struct
  open Twisted_edwards_lib
  module Adder_config = Mixed_add.Config
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
      ; error : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  (* Add a hierarchical wrapper to the adder for multi-SLR. *)
  module Adder = struct
    include Mixed_add

    let hierarchical ~config scope =
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical ~name:"adder" ~scope (create ~config)
    ;;
  end

  module State = struct
    type t =
      | Idle
      | Init_to_identity
      | Working
      | Wait_for_adder
      | Read_result
      | Error
    [@@deriving sexp_of, compare, enumerate]

    let names =
      List.map all ~f:(function
        | Idle -> "I"
        | Init_to_identity -> "i"
        | Working -> "W"
        | Wait_for_adder -> "w"
        | Read_result -> "R"
        | Error -> "E")
    ;;
  end

  let identity_point = Mixed_add.Xyzt.Of_signal.of_ints { x = 0; y = 1; t = 0; z = 1 }

  module Ram_port = struct
    type 'a t =
      { data : 'a [@bits result_point_bits]
      ; read_enable : 'a
      ; write_enable : 'a
      ; address : 'a [@bits last_window_size_bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create
    ~build_mode
    scope
    { I.clock
    ; clear
    ; start
    ; scalar
    ; scalar_valid
    ; last_scalar
    ; input_point
    ; result_point_ready = _ (* TODO Add a fifo and use this.*)
    }
    =
    let ( -- ) = Scope.naming scope in
    Datapa
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
    let ctrl_start = Variable.reg spec ~width:1 in
    let ctrl =
      Controller.hierarchy
        scope
        { Controller.I.clock
        ; clear
        ; start = ctrl_start.value
        ; scalar
        ; scalar_valid
        ; last_scalar
        ; affine_point = input_point
        }
    in
    (* TODO If timing is bad we could potentially pipeline some of these per-RAM *)
    let result_window = pipeline spec ctrl.window ~n:(ram_read_latency + adder_latency) in
    let result_bucket = pipeline spec ctrl.bucket ~n:(ram_read_latency + adder_latency) in
    let adder_valid_in = ctrl.execute &: ~:(ctrl.bubble) in
    let result_write_enable =
      pipeline spec adder_valid_in ~n:(ram_read_latency + adder_latency)
    in
    let ram_port_a = Ram_port.Of_always.reg spec in
    let ram_port_b = Ram_port.Of_always.reg spec in
    let sm = State_machine.create (module State) spec in
    ignore (sm.current -- "STATE" : Signal.t);
    let port_a_q, port_b_q =
      List.init num_windows ~f:(fun window ->
        let address_bits =
          if window = num_windows - 1 then last_window_size_bits else window_size_bits
        in
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
            { write_enable =
                ram_port_a.write_enable.value
                &: (result_window ==:. window |: sm.is Init_to_identity)
            ; read_enable = ram_port_a.read_enable.value
            ; data = ram_port_a.data.value
            ; address = sel_bottom ram_port_a.address.value address_bits
            }
          ~port_b:
            { write_enable = ram_port_b.write_enable.value
            ; read_enable = ram_port_b.read_enable.value
            ; data = ram_port_b.data.value
            ; address = sel_bottom ram_port_b.address.value address_bits
            }
          ())
      |> List.unzip
    in
    let adder =
      Adder.hierarchical
        scope
        ~config:adder_config
        { clock
        ; valid_in = pipeline spec adder_valid_in ~n:ram_read_latency
        ; p1 =
            Mixed_add.Xyzt.Of_signal.unpack
              (mux (pipeline spec ctrl.window ~n:ram_read_latency) port_b_q)
        ; p2 =
            Mixed_add.Xyt.Of_signal.unpack
              (pipeline
                 spec
                 (mux2 ctrl.bubble (ones input_point_bits) ctrl.adder_affine_point)
                 ~n:ram_read_latency)
        }
    in
    (* State machine for error checking and flow control. *)
    let wait_count =
      Variable.reg spec ~width:(num_bits_to_represent (ram_read_latency + adder_latency))
    in
    let window_read_address =
      Variable.reg spec ~width:(num_bits_to_represent num_windows)
    in
    compile
      [ Ram_port.(Of_always.assign ram_port_a (Of_signal.of_int 0))
      ; Ram_port.(Of_always.assign ram_port_b (Of_signal.of_int 0))
      ; sm.switch
          [ Idle, [ when_ start [ sm.set_next Init_to_identity ] ]
          ; ( Init_to_identity
            , [ ram_port_a.write_enable <-- vdd
              ; ram_port_a.data <-- Mixed_add.Xyzt.Of_signal.pack identity_point
              ; when_
                  ram_port_a.write_enable.value
                  [ ram_port_a.address <-- ram_port_a.address.value +:. 1
                  ; when_
                      (ram_port_a.address.value ==:. (1 lsl last_window_size_bits) - 1)
                      [ ctrl_start <-- vdd; sm.set_next Working ]
                  ]
              ] )
          ; ( Working
            , [ ctrl_start <-- gnd
              ; ram_port_a.write_enable <-- result_write_enable
              ; ram_port_a.address <-- result_bucket
              ; ram_port_a.data <-- Mixed_add.Xyzt.Of_signal.pack adder.p3
              ; ram_port_b.write_enable <-- gnd
              ; ram_port_b.address <-- ctrl.bucket
              ; ram_port_b.read_enable <-- vdd
              ; when_ ctrl.done_ [ wait_count <--. 0; sm.set_next Wait_for_adder ]
              ; when_ (result_write_enable <>: adder.valid_out) [ sm.set_next Error ]
              ] )
          ; ( Wait_for_adder
            , [ wait_count <-- wait_count.value +:. 1
              ; when_
                  (wait_count.value ==:. ram_read_latency + adder_latency - 1)
                  [ window_read_address <--. 0
                  ; ram_port_a.address <--. (1 lsl window_size_bits) - 1
                  ; sm.set_next Read_result
                  ]
              ] )
          ; ( Read_result (* TODO: Could optimize logic here better*)
            , [ ram_port_a.read_enable <-- vdd
              ; ram_port_a.address <-- ram_port_a.address.value -:. 1
              ; when_
                  (ram_port_a.address.value ==:. 1)
                  [ window_read_address <-- window_read_address.value +:. 1
                  ; if_
                      (window_read_address.value ==:. num_windows - 2)
                      [ ram_port_a.address <--. (1 lsl last_window_size_bits) - 1 ]
                      [ ram_port_a.address <--. (1 lsl window_size_bits) - 1 ]
                  ; when_
                      (window_read_address.value ==:. num_windows - 1)
                      [ ram_port_a.read_enable <-- gnd; sm.set_next Idle ]
                  ]
              ] )
          ; ( Error
            , [ (* This state is sticky and can only be recovered from with a reset. *) ]
            )
          ]
      ];
    { O.result_point =
        mux (pipeline spec ram_port_a.address.value ~n:ram_read_latency) port_a_q
    ; result_point_valid =
        sm.is Read_result
        &: pipeline spec ram_port_a.read_enable.value ~n:ram_read_latency
    ; scalar_and_input_point_ready = ctrl.scalar_read
    ; error = sm.is Error
    }
  ;;

  let hierarchical ~build_mode scope =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~name:"top" ~scope (create ~build_mode)
  ;;
end
