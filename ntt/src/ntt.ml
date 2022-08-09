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
        ; i : 'a [@bits Int.ceil_log2 logn]
        ; j : 'a [@bits logn]
        ; k : 'a [@bits logn]
        ; m : 'a [@bits logn]
        ; addr1 : 'a [@bits logn]
        ; addr2 : 'a [@bits logn]
        ; omega : 'a [@bits Gf.num_bits]
        ; start_twiddles : 'a
        ; first_stage : 'a
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
                          ; when_ (i_next ==:. logn) [ done_ <--. 1; sm.set_next Idle ]
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
      }
    ;;

    let hierarchy scope =
      let module Hier = Hierarchy.In_scope (I) (O) in
      Hier.hierarchical ~name:"ctrl" ~scope create
    ;;
  end

  module Twiddle_controller = struct
    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; start : 'a
        ; d : 'a [@bits Gf.num_bits]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t =
        { done_ : 'a
        ; addr : 'a
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

    let _create _scope (i : _ I.t) =
      let open Signal in
      let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
      let sm = Always.State_machine.create (module State) spec in
      let j = Var.reg spec ~width:logn in
      let j_next = j.value +:. 1 in
      Always.(
        compile
          [ sm.switch
              [ Idle, [ j <--. 0; when_ i.start [ sm.set_next Looping ] ]
              ; Looping, [ j <-- j_next; when_ (j_next ==:. 0) [ sm.set_next Idle ] ]
              ]
          ]);
      O.Of_signal.of_int 0
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

    let create _scope (i : _ I.t) =
      let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
      (* the latency of the input data must be adjusted to match the latency of the twiddle factor calculation *)
      (* XXX aray: Need to up the latency for a practical version *)
      (* XXX aray: THe pipeline below is timed for 1 cycle ram latency, and 0 cycles twiddle latency *)
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

    let input_ram (i : _ I.t) (core : _ Core.O.t) =
      let q =
        Ram.create
          ~name:"input_ram"
          ~collision_mode:Write_before_read
          ~size:n
          ~write_ports:
            [| { write_clock = i.clock
               ; write_address = i.wr_addr
               ; write_data = i.wr_d
               ; write_enable = i.wr_en
               }
            |]
          ~read_ports:
            [| { read_clock = i.clock
               ; read_address = reverse core.addr1_in
               ; read_enable = core.read_enable_in
               }
             ; { read_clock = i.clock
               ; read_address = reverse core.addr2_in
               ; read_enable = core.read_enable_in
               }
            |]
          ()
      in
      q
    ;;

    let output_ram (i : _ I.t) (core : _ Core.O.t) =
      let q =
        Ram.create
          ~name:"output_ram"
          ~collision_mode:Write_before_read
          ~size:n
          ~write_ports:
            [| { write_clock = i.clock
               ; write_address = core.addr1_out
               ; write_data = core.q1
               ; write_enable = core.write_enable_out
               }
             ; { write_clock = i.clock
               ; write_address = core.addr2_out
               ; write_data = core.q2
               ; write_enable = core.write_enable_out
               }
            |]
          ~read_ports:
            [| { read_clock = i.clock
               ; read_address = core.addr1_in
               ; read_enable = core.read_enable_in
               }
             ; { read_clock = i.clock
               ; read_address = core.addr2_in
               ; read_enable = core.read_enable_in
               }
             ; { read_clock = i.clock; read_address = i.rd_addr; read_enable = i.rd_en }
            |]
          ()
      in
      q
    ;;

    let create scope (i : _ I.t) =
      let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
      let core = Core.O.Of_signal.wires () in
      (* input and output rams *)
      let d_in = input_ram i core in
      let d_out = output_ram i core in
      (* core *)
      let piped_first_stage = pipeline spec ~n:1 core.first_stage in
      Core.O.iter2
        core
        (Core.hierarchy
           scope
           { clock = i.clock
           ; clear = i.clear
           ; start = i.start
           ; d1 = mux2 piped_first_stage d_in.(0) d_out.(0)
           ; d2 = mux2 piped_first_stage d_in.(1) d_out.(1)
           })
        ~f:( <== );
      { O.done_ = core.done_; rd_q = d_out.(2) }
    ;;

    let hierarchy scope =
      let module Hier = Hierarchy.In_scope (I) (O) in
      Hier.hierarchical ~name:"top" ~scope create
    ;;
  end
end
