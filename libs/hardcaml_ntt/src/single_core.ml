open! Base
open! Hardcaml

module Make (Config : Core_config.S) = struct
  open! Signal
  module Var = Always.Variable

  let logn = Config.logn
  let n = 1 lsl logn
  let datapath_latency = Core_config.datapath_latency

  module Controller = Controller.Make (Config)
  module Datapath = Datapath.Make (Config)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; first_iter : 'a
      ; first_4step_pass : 'a
      ; d1 : 'a [@bits Gf.Signal.num_bits]
      ; d2 : 'a [@bits Gf.Signal.num_bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { q1 : 'a [@bits Gf.Signal.num_bits]
      ; q2 : 'a [@bits Gf.Signal.num_bits]
      ; addr1_in : 'a [@bits logn]
      ; addr2_in : 'a [@bits logn]
      ; read_enable_in : 'a
      ; addr1_out : 'a [@bits logn]
      ; addr2_out : 'a [@bits logn]
      ; write_enable_out : 'a
      ; first_stage : 'a
      ; last_stage : 'a
      ; twiddle_stage : 'a
      ; flip : 'a
      ; done_ : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create ?row scope (i : _ I.t) =
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let controller =
      Controller.hierarchy
        scope
        { Controller.I.clock = i.clock
        ; clear = i.clear
        ; start = i.start
        ; first_iter = i.first_iter
        ; first_4step_pass = i.first_4step_pass
        }
    in
    let datapath =
      Datapath.hierarchy
        ?row
        scope
        { Datapath.I.clock = i.clock
        ; clear = i.clear
        ; start = i.start
        ; first_iter = i.first_iter
        ; d1 = i.d1
        ; d2 = i.d2
        ; omegas = controller.omegas
        ; start_twiddles = controller.start_twiddles
        ; twiddle_stage = controller.twiddle_stage
        ; twiddle_update = controller.twiddle_update
        }
    in
    let pipe = pipeline spec ~n:(datapath_latency + 1) in
    { O.q1 = datapath.q1
    ; q2 = datapath.q2
    ; addr1_in = controller.addr1
    ; addr2_in = controller.addr2
    ; read_enable_in = controller.read_write_enable
    ; addr1_out = controller.addr1 |> pipe
    ; addr2_out = controller.addr2 |> pipe
    ; write_enable_out = controller.read_write_enable |> pipe
    ; first_stage = controller.first_stage
    ; last_stage = controller.last_stage
    ; twiddle_stage = controller.twiddle_stage
    ; flip = controller.flip
    ; done_ = controller.done_
    }
  ;;

  let hierarchy ?row scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~name:"core" ~scope (create ?row)
  ;;
end

module With_rams (Config : Core_config.S) = struct
  open! Signal

  let logn = Config.logn
  let n = 1 lsl logn
  let datapath_latency = Core_config.datapath_latency
  let ram_latency = Core_config.ram_latency

  module Core = Make (Config)

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; first_4step_pass : 'a
      ; first_iter : 'a
      ; flip : 'a
      ; wr_d : 'a [@bits Gf.Signal.num_bits]
      ; wr_en : 'a
      ; wr_addr : 'a [@bits logn]
      ; rd_en : 'a
      ; rd_addr : 'a [@bits logn]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { done_ : 'a
      ; rd_q : 'a [@bits Gf.Signal.num_bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let input_ram
    scope
    build_mode
    ~clock
    ~clear
    ~flip
    ~wr_addr
    ~wr_d
    ~wr_en
    ~addr1_in
    ~addr2_in
    ~read_enable_in
    =
    Bram.create_dual
      (Scope.sub_scope scope "ram_in")
      ~build_mode
      ~size:n
      ~read_latency:ram_latency
      ~clock
      ~clear
      ~flip
      ~write_port_a:{ address = wr_addr; data = wr_d; enable = wr_en }
      ~write_port_b:{ address = zero logn; data = zero Gf.Signal.num_bits; enable = gnd }
      ~read_port_a:{ address = reverse addr1_in; enable = read_enable_in }
      ~read_port_b:{ address = reverse addr2_in; enable = read_enable_in }
  ;;

  let transpose_ram
    scope
    build_mode
    ~clock
    ~clear
    ~addr1_in
    ~addr1_out
    ~q1
    ~addr2_in
    ~addr2_out
    ~q2
    ~read_enable_in
    ~write_enable_out
    ~flip
    ~last_stage
    =
    let scope = Scope.sub_scope scope "ram_transp" in
    let ( -- ) = Scope.naming scope in
    let q0, q1 =
      Bram.create_dual
        scope
        ~build_mode
        ~size:n
        ~read_latency:ram_latency
        ~clock
        ~clear
        ~flip
        ~write_port_a:
          { address = addr1_out; data = q1; enable = write_enable_out &: ~:last_stage }
        ~write_port_b:
          { address = addr2_out; data = q2; enable = write_enable_out &: ~:last_stage }
        ~read_port_a:{ address = addr1_in; enable = read_enable_in }
        ~read_port_b:{ address = addr2_in; enable = read_enable_in }
    in
    q0 -- "q0", q1 -- "q1"
  ;;

  let output_ram
    scope
    build_mode
    ~clock
    ~clear
    ~flip
    ~last_stage
    ~twiddle_stage
    ~rd_addr
    ~rd_en
    ~addr1_out
    ~q1
    ~addr2_out
    ~q2
    ~write_enable_out
    =
    let q, _ =
      Bram.create_dual
        (Scope.sub_scope scope "ram_out")
        ~build_mode
        ~size:n
        ~read_latency:ram_latency
        ~clock
        ~clear
        ~flip
        ~write_port_a:
          { address = addr1_out
          ; data = q1
          ; enable = write_enable_out &: (last_stage &: ~:twiddle_stage)
          }
        ~write_port_b:
          { address = addr2_out; data = q2; enable = write_enable_out &: last_stage }
        ~read_port_a:{ address = rd_addr; enable = rd_en }
        ~read_port_b:{ address = zero logn; enable = gnd }
    in
    q
  ;;

  let create ?row ~build_mode scope (i : _ I.t) =
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let core = Core.O.Of_signal.wires () in
    let pipe ~n d = pipeline spec ~n d in
    (* input and output rams *)
    let d_in_0, d_in_1 =
      input_ram
        scope
        build_mode
        ~clock:i.clock
        ~clear:i.clear
        ~flip:i.flip
        ~wr_addr:i.wr_addr
        ~wr_d:i.wr_d
        ~wr_en:i.wr_en
        ~addr1_in:core.addr1_in
        ~addr2_in:core.addr2_in
        ~read_enable_in:core.read_enable_in
    in
    let d_out_0, d_out_1 =
      transpose_ram
        scope
        build_mode
        ~clock:i.clock
        ~clear:i.clear
        ~addr1_in:core.addr1_in
        ~addr1_out:core.addr1_out
        ~q1:core.q1
        ~addr2_in:core.addr2_in
        ~addr2_out:core.addr2_out
        ~q2:core.q2
        ~read_enable_in:core.read_enable_in
        ~write_enable_out:core.write_enable_out
        ~flip:core.flip
        ~last_stage:core.last_stage
    in
    (* core *)
    Core.O.iter2
      core
      (Core.hierarchy
         ?row
         scope
         (let first_stage = pipe ~n:Core_config.ram_latency core.first_stage in
          { clock = i.clock
          ; clear = i.clear
          ; start = i.start
          ; first_iter = i.first_iter
          ; first_4step_pass = i.first_4step_pass
          ; d1 = mux2 first_stage d_in_0 d_out_0
          ; d2 = mux2 first_stage d_in_1 d_out_1
          }))
      ~f:( <== );
    let d_out =
      output_ram
        scope
        build_mode
        ~clock:i.clock
        ~clear:i.clear
        ~flip:i.flip
        ~last_stage:(pipe ~n:(datapath_latency + 1) core.last_stage)
        ~twiddle_stage:(pipe ~n:(datapath_latency + 1) core.twiddle_stage)
        ~rd_addr:i.rd_addr
        ~rd_en:i.rd_en
        ~addr1_out:core.addr1_out
        ~q1:core.q1
        ~addr2_out:core.addr2_out
        ~q2:core.q2
        ~write_enable_out:core.write_enable_out
    in
    { O.done_ = core.done_; rd_q = d_out }
  ;;

  let hierarchy ?row ?instance ~build_mode scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~name:"ntt_with_rams" ?instance ~scope (create ?row ~build_mode)
  ;;
end
