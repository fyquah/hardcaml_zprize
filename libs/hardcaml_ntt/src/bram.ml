open! Base
open Hardcaml
open Signal

let create scope ~build_mode ~size ~read_latency ~clock ~port_a ~port_b =
  let port scope p =
    let ( -- ) = Scope.naming scope in
    Hardcaml_xilinx.Ram_port.(map2 p port_names ~f:( -- ))
  in
  Hardcaml_xilinx.True_dual_port_ram.create
    ~read_latency
    ~build_mode
    ~arch:Ultraram
    ()
    ~clock_a:clock
    ~clock_b:clock
    ~clear_a:gnd
    ~clear_b:gnd
    ~size
    ~port_a:(port (Scope.sub_scope scope "a") port_a)
    ~port_b:(port (Scope.sub_scope scope "b") port_b)
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
  scope
  ~build_mode
  ~size
  ~read_latency
  ~clock
  ~clear
  ~flip
  ~(write_port_a : write_port)
  ~(write_port_b : write_port)
  ~(read_port_a : read_port)
  ~(read_port_b : read_port)
  =
  let ( -- ) = Scope.naming scope in
  let spec = Reg_spec.create ~clock ~clear () in
  let phase = reg_fb spec ~width:1 ~enable:flip ~f:( ~: ) -- "PHASE" in
  let create scope phase =
    create
      scope
      ~build_mode
      ~size
      ~clock
      ~read_latency
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
  let q0, q1 = create (Scope.sub_scope scope "a") phase in
  let q2, q3 = create (Scope.sub_scope scope "b") ~:phase in
  mux2 phase q2 q0, mux2 phase q3 q1
;;
