open! Base
open Hardcaml
open Signal

let create ~build_mode ~size ~clock ~port_a ~port_b =
  Hardcaml_xilinx.True_dual_port_ram.create
    ~build_mode
    ()
    ~clock_a:clock
    ~clock_b:clock
    ~clear_a:gnd
    ~clear_b:gnd
    ~size
    ~port_a
    ~port_b
;;

type write_port =
  { address : Signal.t
  ; data : Signal.t
  ; enable : Signal.t
  }

type read_port =
  { address : Signal.t
  ; enable : Signal.t
  }

let create_dual
    ~build_mode
    ~size
    ~clock
    ~clear
    ~flip
    ~(write_port_a : write_port)
    ~(write_port_b : write_port)
    ~(read_port_a : read_port)
    ~(read_port_b : read_port)
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let phase = reg_fb spec ~width:1 ~enable:flip ~f:( ~: ) in
  let create phase =
    create
      ~build_mode
      ~size
      ~clock
      ~port_a:
        { address = mux2 phase write_port_a.address read_port_a.address
        ; data = write_port_a.data
        ; write_enable = write_port_a.enable &: phase
        ; read_enable = read_port_a.enable &: ~:phase
        }
      ~port_b:
        { address = mux2 phase write_port_b.address read_port_b.address
        ; data = write_port_b.data
        ; write_enable = write_port_b.enable &: phase
        ; read_enable = read_port_b.enable &: ~:phase
        }
  in
  let q0, q1 = create phase in
  let q2, q3 = create ~:phase in
  mux2 phase q2 q0, mux2 phase q3 q1
;;
