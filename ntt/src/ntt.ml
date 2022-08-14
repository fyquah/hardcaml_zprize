open! Base
open! Hardcaml

(* Max transform size*)

module type Size = sig
  val logn : int
end

module Gf = Gf_bits.Make (Hardcaml.Signal)

module Make (P : Size) = struct
  let logn = P.logn
  let n = 1 lsl logn

  module Controller = struct
    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; start : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t =
        { done_ : 'a
        ; i : 'a [@bits Int.ceil_log2 (logn + 1)]
        ; j : 'a [@bits logn]
        ; k : 'a [@bits logn]
        ; m : 'a [@bits logn]
        ; addr1 : 'a [@bits logn]
        ; addr2 : 'a [@bits logn]
        ; omega : 'a [@bits Gf.num_bits]
        ; start_twiddles : 'a
        ; first_stage : 'a
        ; last_stage : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module State = struct
      type t =
        | Idle
        | Looping
      [@@deriving compare, enumerate, sexp_of, variants]
    end

    module Var = Always.Variable

    let create _scope (inputs : _ I.t) =
      let open Signal in
      let spec = Reg_spec.create ~clock:inputs.clock ~clear:inputs.clear () in
      let sm = Always.State_machine.create (module State) spec in
      let done_ = Var.reg (Reg_spec.override spec ~clear_to:vdd) ~width:1 in
      let i = Var.reg spec ~width:(Int.ceil_log2 (logn + 1)) in
      let i_next = i.value +:. 1 in
      let j = Var.reg spec ~width:logn in
      let j_next = j.value +:. 1 in
      let k = Var.reg spec ~width:logn in
      let m = Var.reg spec ~width:logn in
      let m_next = sll m.value 1 in
      let k_next = k.value +: m_next in
      let addr1 = Var.reg spec ~width:logn in
      let addr2 = Var.reg spec ~width:logn in
      let omega =
        List.init logn ~f:(fun i ->
            Roots.inverse.(i + 1) |> Gf_z.to_z |> Gf.of_z |> Gf.to_bits)
        |> mux i.value
      in
      let start_twiddles = Var.reg spec ~width:1 in
      let first_stage = Var.reg spec ~width:1 in
      let last_stage = Var.reg spec ~width:1 in
      Always.(
        compile
          [ start_twiddles <--. 0
          ; sm.switch
              [ ( Idle
                , [ when_
                      inputs.start
                      [ i <--. 0
                      ; j <--. 0
                      ; k <--. 0
                      ; m <--. 1
                      ; addr1 <--. 0
                      ; addr2 <--. 1
                      ; done_ <--. 0
                      ; start_twiddles <--. 1
                      ; first_stage <--. 1
                      ; sm.set_next Looping
                      ]
                  ] )
              ; ( Looping
                , [ j <-- j_next
                  ; addr1 <-- addr1.value +:. 1
                  ; addr2 <-- addr2.value +:. 1
                  ; (* perform the butterfly here.*)
                    when_
                      (j_next ==: m.value)
                      [ j <--. 0
                      ; start_twiddles <--. 1
                      ; k <-- k_next
                      ; addr1 <-- k_next
                      ; addr2 <-- k_next +: m.value
                      ; when_
                          (k_next ==:. 0)
                          [ i <-- i_next
                          ; first_stage <--. 0
                          ; m <-- m_next
                          ; addr2 <-- k_next +: m_next
                          ; when_ (i_next ==:. logn - 1) [ last_stage <--. 1 ]
                          ; when_
                              (i_next ==:. logn)
                              [ done_ <--. 1; last_stage <--. 0; sm.set_next Idle ]
                          ]
                      ]
                  ] )
              ]
          ]);
      { O.done_ = done_.value
      ; i = i.value
      ; j = j.value
      ; k = k.value
      ; m = m.value
      ; addr1 = addr1.value
      ; addr2 = addr2.value
      ; omega
      ; start_twiddles = start_twiddles.value
      ; first_stage = first_stage.value
      ; last_stage = last_stage.value
      }
    ;;

    let hierarchy scope =
      let module Hier = Hierarchy.In_scope (I) (O) in
      Hier.hierarchical ~name:"ctrl" ~scope create
    ;;
  end

  (* Butterly and twiddle factor calculation *)
  module Datapath = struct
    open Signal

    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; d1 : 'a [@bits Gf.num_bits]
        ; d2 : 'a [@bits Gf.num_bits]
        ; omega : 'a [@bits Gf.num_bits]
        ; start_twiddles : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t =
        { q1 : 'a [@bits Gf.num_bits]
        ; q2 : 'a [@bits Gf.num_bits]
        }
      [@@deriving sexp_of, hardcaml]
    end

    let ( *: ) a b = Gf.( * ) (Gf.of_bits a) (Gf.of_bits b) |> Gf.to_bits
    let ( +: ) a b = Gf.( + ) (Gf.of_bits a) (Gf.of_bits b) |> Gf.to_bits
    let ( -: ) a b = Gf.( - ) (Gf.of_bits a) (Gf.of_bits b) |> Gf.to_bits

    let twiddle_factor (i : _ I.t) w =
      mux2 i.start_twiddles Gf.(to_bits one) (w *: i.omega)
    ;;

    let create scope (i : _ I.t) =
      let ( -- ) = Scope.naming scope in
      let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
      (* the latency of the input data must be adjusted to match the latency of the twiddle factor calculation *)
      (* XXX aray: Need to up the latency for a practical version *)
      (* XXX aray: The pipeline below is timed for 1 cycle ram latency, and 0 cycles twiddle latency *)
      let w = wire Gf.num_bits -- "twiddle" in
      w <== reg spec (twiddle_factor i w);
      let t = i.d2 *: w in
      { O.q1 = i.d1 +: t; q2 = i.d1 -: t }
    ;;

    let hierarchy scope =
      let module Hier = Hierarchy.In_scope (I) (O) in
      Hier.hierarchical ~name:"dp" ~scope create
    ;;
  end

  module Core = struct
    open! Signal

    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; start : 'a
        ; d1 : 'a [@bits Gf.num_bits]
        ; d2 : 'a [@bits Gf.num_bits]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t =
        { q1 : 'a [@bits Gf.num_bits]
        ; q2 : 'a [@bits Gf.num_bits]
        ; addr1_in : 'a [@bits logn]
        ; addr2_in : 'a [@bits logn]
        ; read_enable_in : 'a
        ; addr1_out : 'a [@bits logn]
        ; addr2_out : 'a [@bits logn]
        ; write_enable_out : 'a
        ; first_stage : 'a
        ; last_stage : 'a
        ; done_ : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    let create scope (i : _ I.t) =
      let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
      let controller =
        Controller.hierarchy
          scope
          { Controller.I.clock = i.clock; clear = i.clear; start = i.start }
      in
      let datapath =
        Datapath.hierarchy
          scope
          { Datapath.I.clock = i.clock
          ; clear = i.clear
          ; d1 = i.d1
          ; d2 = i.d2
          ; omega = controller.omega
          ; start_twiddles = controller.start_twiddles
          }
      in
      let pipe = pipeline spec ~n:1 in
      { O.q1 = datapath.q1
      ; q2 = datapath.q2
      ; addr1_in = controller.addr1
      ; addr2_in = controller.addr2
      ; read_enable_in = ~:(controller.done_)
      ; addr1_out = controller.addr1 |> pipe
      ; addr2_out = controller.addr2 |> pipe
      ; write_enable_out = ~:(controller.done_) |> pipe
      ; first_stage = controller.first_stage
      ; last_stage = controller.last_stage
      ; done_ = controller.done_
      }
    ;;

    let hierarchy scope =
      let module Hier = Hierarchy.In_scope (I) (O) in
      Hier.hierarchical ~name:"core" ~scope create
    ;;
  end

  module With_rams = struct
    open! Signal

    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; start : 'a
        ; flip : 'a
        ; wr_d : 'a [@bits Gf.num_bits]
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
        ; rd_q : 'a [@bits Gf.num_bits]
        }
      [@@deriving sexp_of, hardcaml]
    end

    let input_ram build_mode (i : _ I.t) (core : _ Core.O.t) =
      Bram.create_dual
        ~build_mode
        ~size:n
        ~clock:i.clock
        ~clear:i.clear
        ~flip:i.flip
        ~write_port_a:{ address = i.wr_addr; data = i.wr_d; enable = i.wr_en }
        ~write_port_b:{ address = zero logn; data = zero Gf.num_bits; enable = gnd }
        ~read_port_a:{ address = reverse core.addr1_in; enable = core.read_enable_in }
        ~read_port_b:{ address = reverse core.addr2_in; enable = core.read_enable_in }
    ;;

    let transpose_ram _build_mode (i : _ I.t) (core : _ Core.O.t) =
      let phase_i =
        reg_fb
          (Reg_spec.create ~clock:i.clock ~clear:i.clear ())
          ~width:1
          ~enable:i.flip
          ~f:( ~: )
      in
      let phase_o = ~:phase_i in
      let q =
        Ram.create
          ~collision_mode:Write_before_read
          ~size:(n * 2)
          ~write_ports:
            [| { write_clock = i.clock
               ; write_address = phase_i @: core.addr1_out
               ; write_data = core.q1
               ; write_enable = core.write_enable_out
               }
             ; { write_clock = i.clock
               ; write_address = phase_i @: core.addr2_out
               ; write_data = core.q2
               ; write_enable = core.write_enable_out
               }
            |]
          ~read_ports:
            [| { read_clock = i.clock
               ; read_address = phase_i @: core.addr1_in
               ; read_enable = core.read_enable_in
               }
             ; { read_clock = i.clock
               ; read_address = phase_i @: core.addr2_in
               ; read_enable = core.read_enable_in
               }
             ; { read_clock = i.clock
               ; read_address = phase_o @: i.rd_addr
               ; read_enable = i.rd_en
               }
            |]
          ()
      in
      q.(0), q.(1), q.(2)
    ;;

    let output_ram build_mode (i : _ I.t) (core : _ Core.O.t) ~last_stage =
      let q, _ =
        Bram.create_dual
          ~build_mode
          ~size:n
          ~clock:i.clock
          ~clear:i.clear
          ~flip:i.flip
          ~write_port_a:
            { address = core.addr1_out
            ; data = core.q1
            ; enable = core.write_enable_out &: last_stage
            }
          ~write_port_b:
            { address = core.addr2_out
            ; data = core.q2
            ; enable = core.write_enable_out &: last_stage
            }
          ~read_port_a:{ address = i.rd_addr; enable = i.rd_en }
          ~read_port_b:{ address = zero logn; enable = gnd }
      in
      q
    ;;

    let create ~build_mode scope (i : _ I.t) =
      let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
      let core = Core.O.Of_signal.wires () in
      (* input and output rams *)
      let d_in_0, d_in_1 = input_ram build_mode i core in
      let d_out_0, d_out_1, _d_out = transpose_ram build_mode i core in
      (* core *)
      let piped_first_stage = pipeline spec ~n:1 core.first_stage in
      let piped_last_stage = pipeline spec ~n:1 core.last_stage in
      Core.O.iter2
        core
        (Core.hierarchy
           scope
           { clock = i.clock
           ; clear = i.clear
           ; start = i.start
           ; d1 = mux2 piped_first_stage d_in_0 d_out_0
           ; d2 = mux2 piped_first_stage d_in_1 d_out_1
           })
        ~f:( <== );
      let d_out = output_ram build_mode i core ~last_stage:piped_last_stage in
      { O.done_ = core.done_; rd_q = d_out }
    ;;

    let hierarchy ?instance ~build_mode scope =
      let module Hier = Hierarchy.In_scope (I) (O) in
      Hier.hierarchical ~name:"ntt_with_rams" ?instance ~scope (create ~build_mode)
    ;;
  end
end
